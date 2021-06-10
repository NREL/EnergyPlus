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
#include <cassert>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/numeric.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/CostEstimateManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EconomicTariff.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/ResultsFramework.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::EconomicTariff {

// MODULE INFORMATION:
//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
//    DATE WRITTEN   May 2004

//    Compute utility bills for a building based on energy
//    use estimate.
using ScheduleManager::GetScheduleIndex;

void UpdateUtilityBills(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   September 2003

    //    Single routine used to call all get input
    //    routines for economics.

    using OutputReportTabular::AddTOCEntry;

    bool ErrorsFound(false);

    if (state.dataEconTariff->Update_GetInput) {
        GetInputEconomicsTariff(state, ErrorsFound);
        // do rest of GetInput only if at least one tariff is defined.
        GetInputEconomicsCurrencyType(state, ErrorsFound);
        if (state.dataEconTariff->numTariff >= 1) {
            if (!ErrorsFound && state.dataOutRptTab->displayEconomicResultSummary)
                AddTOCEntry(state, "Economics Results Summary Report", "Entire Facility");
            CreateCategoryNativeVariables(state);
            GetInputEconomicsQualify(state, ErrorsFound);
            GetInputEconomicsChargeSimple(state, ErrorsFound);
            GetInputEconomicsChargeBlock(state, ErrorsFound);
            GetInputEconomicsRatchet(state, ErrorsFound);
            GetInputEconomicsVariable(state, ErrorsFound);
            GetInputEconomicsComputation(state, ErrorsFound);
            CreateDefaultComputation(state);
        }
        state.dataEconTariff->Update_GetInput = false;
        if (ErrorsFound) ShowFatalError(state, "UpdateUtilityBills: Preceding errors cause termination.");
    }
    if (state.dataGlobal->DoOutputReporting && (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather)) {
        GatherForEconomics(state);
    }
}

//======================================================================================================================
//======================================================================================================================

//    GET INPUT ROUTINES

//======================================================================================================================
//======================================================================================================================

void GetInputEconomicsTariff(EnergyPlusData &state, bool &ErrorsFound) // true if errors found during getting input objects.
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //       DATE WRITTEN   May 2004
    //       MODIFIED       Aug. 2017, Julien Marrec of EffiBEM. Handled conversions factor based on meter resources
    //
    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine reads the input file for "UtilityCost:Tariff" objects
    // It will be the right conversion factors based on the associated meter resource type
    // meaning if "CCF" is picked, the conversion factor isn't the same whether it's a water meter or a fuel meter.

    using DataGlobalConstants::AssignResourceTypeNum;
    using OutputReportTabular::AddTOCEntry;

    std::string const RoutineName("GetInputEconomicsTariff: ");
    int iInObj;    // loop index variable for reading in objects
    int jObj;      // loop index for objects
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    int found;
    bool isNotNumeric;
    // variables for getting report variable/meter index
    int KeyCount;
    OutputProcessor::VariableType TypeVar;
    OutputProcessor::StoreType AvgSumVar;
    OutputProcessor::TimeStepType StepTypeVar;
    OutputProcessor::Unit UnitsVar(OutputProcessor::Unit::None); // Units sting, may be blank
    Array1D_string NamesOfKeys;                                  // Specific key name
    Array1D_int IndexesForKeyVar;                                // Array index
    int jFld;
    std::string CurrentModuleObject; // for ease in renaming.

    auto &tariff(state.dataEconTariff->tariff);

    CurrentModuleObject = "UtilityCost:Tariff";
    state.dataEconTariff->numTariff = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    tariff.allocate(state.dataEconTariff->numTariff);
    for (iInObj = 1; iInObj <= state.dataEconTariff->numTariff; ++iInObj) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 iInObj,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another economic object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            //  args are always turned to upper case but this is okay...
            if (hasi(state.dataIPShortCut->cAlphaArgs(jFld), "UtilityCost:")) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\".");
                ShowContinueError(state, "... a field was found containing UtilityCost: which may indicate a missing comma.");
            }
        }
        // name of the tariff
        tariff(iInObj).tariffName = state.dataIPShortCut->cAlphaArgs(1);
        // check if tariff name is unique
        found = 0;
        for (jObj = 1; jObj <= iInObj - 1; ++jObj) {
            if (tariff(iInObj).tariffName == tariff(jObj).tariffName) {
                found = jObj;
                break;
            }
        }
        if (found > 0) {
            ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
            ShowContinueError(state, "...Duplicate name. Name has already been used.");
            ErrorsFound = true;
        }
        // name of the report meter
        tariff(iInObj).reportMeter = state.dataIPShortCut->cAlphaArgs(2);
        // call the key count function but only need count during this pass
        GetVariableKeyCountandType(state, tariff(iInObj).reportMeter, KeyCount, TypeVar, AvgSumVar, StepTypeVar, UnitsVar);
        // if no meters found for that name
        if (KeyCount == 0) {
            ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" missing meter");
            ShowContinueError(state,
                              "Meter referenced is not present due to a lack of equipment that uses that energy source/meter:\"" +
                                  tariff(iInObj).reportMeter + "\".");
            tariff(iInObj).reportMeterIndx = 0;
        } else {
            NamesOfKeys.allocate(KeyCount);
            IndexesForKeyVar.allocate(KeyCount);
            GetVariableKeys(state, tariff(iInObj).reportMeter, TypeVar, NamesOfKeys, IndexesForKeyVar);
            // although this retrieves all keys for a variable, we only need one so the first one is chosen
            if (KeyCount > 1) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" multiple keys");
                ShowContinueError(state, "... Multiple keys for variable select. First key will be used.");
            }
            // assign the index
            tariff(iInObj).reportMeterIndx = IndexesForKeyVar(1);
            // get rid of the arrays used to get the variable number
            NamesOfKeys.deallocate();
            IndexesForKeyVar.deallocate();
        }

        // Start by checking what type of meter we do have, some units can be used for several resources with different conversion factors
        // Explicitly assume it's not a water meter nor an electric meter nor a gas meter (was already done in constructor though)
        tariff(iInObj).kindWaterMtr = kindMeterNotWater;
        tariff(iInObj).kindElectricMtr = kindMeterNotElectric;
        tariff(iInObj).kindGasMtr = kindMeterNotGas;

        // Determine whether this meter is related to electricity, or water, or gas
        if (tariff(iInObj).reportMeterIndx != 0) {

            auto const SELECT_CASE_var(
                UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->EnergyMeters(tariff(iInObj).reportMeterIndx).ResourceType));

            // Various types of electricity meters
            if (SELECT_CASE_var == "ELECTRICITY") {
                tariff(iInObj).kindElectricMtr = kindMeterElecSimple;
            } else if (SELECT_CASE_var == "ELECTRICITYPRODUCED") {
                tariff(iInObj).kindElectricMtr = kindMeterElecProduced;
            } else if (SELECT_CASE_var == "ELECTRICITYPURCHASED") {
                tariff(iInObj).kindElectricMtr = kindMeterElecPurchased;
            } else if (SELECT_CASE_var == "ELECTRICITYSURPLUSSOLD") {
                tariff(iInObj).kindElectricMtr = kindMeterElecSurplusSold;
            } else if (SELECT_CASE_var == "ELECTRICITYNET") {
                tariff(iInObj).kindElectricMtr = kindMeterElecNet;

                // Handle the case where its a water meter
            } else if (SELECT_CASE_var == "WATER" || SELECT_CASE_var == "H2O" || SELECT_CASE_var == "ONSITEWATER" ||
                       SELECT_CASE_var == "WATERPRODUCED" || SELECT_CASE_var == "ONSITE WATER" || SELECT_CASE_var == "MAINSWATER" ||
                       SELECT_CASE_var == "WATERSUPPLY" || SELECT_CASE_var == "RAINWATER" || SELECT_CASE_var == "PRECIPITATION" ||
                       SELECT_CASE_var == "WELLWATER" || SELECT_CASE_var == "GROUNDWATER" || SELECT_CASE_var == "CONDENSATE") {
                tariff(iInObj).kindWaterMtr = kindMeterWater;
                // Or a Natural Gas meter
            } else if (SELECT_CASE_var == "GAS" || SELECT_CASE_var == "NATURALGAS") {
                tariff(iInObj).kindGasMtr = kindMeterGas;
            }
        }

        // Assign the right conversion factors based on the resource type

        // If it's a water meter
        // We set demandConv to something analogous to m3/h
        if (tariff(iInObj).kindWaterMtr == kindMeterWater) {
            // conversion factor
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "USERDEFINED")) {
                tariff(iInObj).convChoice = iEconConv::USERDEF;
                tariff(iInObj).energyConv = state.dataIPShortCut->rNumericArgs(1); // energy conversion factor
                tariff(iInObj).demandConv = state.dataIPShortCut->rNumericArgs(2); // demand conversion factor
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "M3")) {
                tariff(iInObj).convChoice = iEconConv::M3;
                tariff(iInObj).energyConv = 1.0;
                tariff(iInObj).demandConv = 3600.0;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "CCF")) {
                tariff(iInObj).convChoice = iEconConv::CCF;
                tariff(iInObj).energyConv = 0.35314666721488586;
                tariff(iInObj).demandConv = 0.35314666721488586 * 3600;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "GAL")) {
                tariff(iInObj).convChoice = iEconConv::GAL;
                tariff(iInObj).energyConv = 264.1720523602524;
                tariff(iInObj).demandConv = 264.1720523602524 * 3600;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "KGAL")) {
                tariff(iInObj).convChoice = iEconConv::KGAL;
                tariff(iInObj).energyConv = 0.2641720523602524;
                tariff(iInObj).demandConv = 0.2641720523602524 * 3600;
            } else {
                // ERROR: not a valid conversion, default to M3
                tariff(iInObj).convChoice = iEconConv::M3;
                tariff(iInObj).energyConv = 1.0;
                tariff(iInObj).demandConv = 3600.0;
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) +
                                      "\", Defaulting to m^3 (Water resource detected).");
            }

            // If it's an electric meter
            // Volumetric units such as MCF or CCF doesn't make sense IMHO (JM)
            // THERM is strange for an electric meter but currently I accept but issue a warning
        } else if (tariff(iInObj).kindElectricMtr != kindMeterNotElectric) {
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "USERDEFINED")) {
                tariff(iInObj).convChoice = iEconConv::USERDEF;
                tariff(iInObj).energyConv = state.dataIPShortCut->rNumericArgs(1); // energy conversion factor
                tariff(iInObj).demandConv = state.dataIPShortCut->rNumericArgs(2); // demand conversion factor
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "KWH")) {
                tariff(iInObj).convChoice = iEconConv::KWH;
                tariff(iInObj).energyConv = 0.0000002778;
                tariff(iInObj).demandConv = 0.001;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "MJ")) {
                tariff(iInObj).convChoice = iEconConv::MJ;
                tariff(iInObj).energyConv = 0.000001;
                tariff(iInObj).demandConv = 0.0036;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "MMBTU")) {
                tariff(iInObj).convChoice = iEconConv::MMBTU;
                tariff(iInObj).energyConv = 9.4781712e-10;
                tariff(iInObj).demandConv = 0.000003412;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "KBTU")) {
                tariff(iInObj).convChoice = iEconConv::KBTU;
                tariff(iInObj).energyConv = 9.4781712e-7;
                tariff(iInObj).demandConv = 0.003412;

                // We accept the following choices, but issue a warning
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "THERM")) {
                tariff(iInObj).convChoice = iEconConv::THERM;
                tariff(iInObj).energyConv = 9.4781712e-9;
                tariff(iInObj).demandConv = 0.00003412;
                ShowWarningError(state,
                                 RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" potentially invalid data");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) +
                                      "\", Therm is an unusual choice for an electric resource.)");

                // Otherwise, default to kWh
            } else {
                tariff(iInObj).convChoice = iEconConv::KWH;
                tariff(iInObj).energyConv = 0.0000002778;
                tariff(iInObj).demandConv = 0.001;
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) +
                                      "\", Defaulting to kWh (Electric resource detected)");
            }

            // If it's a gas meter
        } else if (tariff(iInObj).kindGasMtr == kindMeterGas) {
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "USERDEFINED")) {
                tariff(iInObj).convChoice = iEconConv::USERDEF;
                tariff(iInObj).energyConv = state.dataIPShortCut->rNumericArgs(1); // energy conversion factor
                tariff(iInObj).demandConv = state.dataIPShortCut->rNumericArgs(2); // demand conversion factor
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "KWH")) {
                tariff(iInObj).convChoice = iEconConv::KWH;
                tariff(iInObj).energyConv = 0.0000002778;
                tariff(iInObj).demandConv = 0.001;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "THERM")) {
                tariff(iInObj).convChoice = iEconConv::THERM;
                tariff(iInObj).energyConv = 9.4781712e-9;
                tariff(iInObj).demandConv = 0.00003412;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "MMBTU")) {
                tariff(iInObj).convChoice = iEconConv::MMBTU;
                tariff(iInObj).energyConv = 9.4781712e-10;
                tariff(iInObj).demandConv = 0.000003412;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "MJ")) {
                tariff(iInObj).convChoice = iEconConv::MJ;
                tariff(iInObj).energyConv = 0.000001;
                tariff(iInObj).demandConv = 0.0036;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "KBTU")) {
                tariff(iInObj).convChoice = iEconConv::KBTU;
                tariff(iInObj).energyConv = 9.4781712e-7;
                tariff(iInObj).demandConv = 0.003412;

                // Volumetric units for natural gas
                // Actually assuming 1 therm = 1 CCF (= 100 ft^3)
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "MCF")) {
                tariff(iInObj).convChoice = iEconConv::MCF;
                tariff(iInObj).energyConv = 9.4781712e-10;
                tariff(iInObj).demandConv = 0.000003412;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "CCF")) {
                tariff(iInObj).convChoice = iEconConv::CCF;
                tariff(iInObj).energyConv = 9.4781712e-9;
                tariff(iInObj).demandConv = 0.00003412;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "M3")) {
                // Obtained from converting CCF above to m^3 so the same heat content of natural gas is used (1 therm = 1 CCF)
                tariff(iInObj).convChoice = iEconConv::M3;
                tariff(iInObj).energyConv = 2.6839192e-10;
                tariff(iInObj).demandConv = 9.6617081E-05;

                // Otherwise, default to kWh
            } else {
                tariff(iInObj).convChoice = iEconConv::KWH;
                tariff(iInObj).energyConv = 0.0000002778;
                tariff(iInObj).demandConv = 0.001;
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\", Defaulting to kWh.");
            }

            // It it's neither an electric, water or gas meter, we cannot accept volumetric units
            // because we cannot infer the heat content
        } else {
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "USERDEFINED")) {
                tariff(iInObj).convChoice = iEconConv::USERDEF;
                tariff(iInObj).energyConv = state.dataIPShortCut->rNumericArgs(1); // energy conversion factor
                tariff(iInObj).demandConv = state.dataIPShortCut->rNumericArgs(2); // demand conversion factor
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "KWH")) {
                tariff(iInObj).convChoice = iEconConv::KWH;
                tariff(iInObj).energyConv = 0.0000002778;
                tariff(iInObj).demandConv = 0.001;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "THERM")) {
                tariff(iInObj).convChoice = iEconConv::THERM;
                tariff(iInObj).energyConv = 9.4781712e-9;
                tariff(iInObj).demandConv = 0.00003412;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "MMBTU")) {
                tariff(iInObj).convChoice = iEconConv::MMBTU;
                tariff(iInObj).energyConv = 9.4781712e-10;
                tariff(iInObj).demandConv = 0.000003412;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "MJ")) {
                tariff(iInObj).convChoice = iEconConv::MJ;
                tariff(iInObj).energyConv = 0.000001;
                tariff(iInObj).demandConv = 0.0036;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "KBTU")) {
                tariff(iInObj).convChoice = iEconConv::KBTU;
                tariff(iInObj).energyConv = 9.4781712e-7;
                tariff(iInObj).demandConv = 0.003412;

                // Otherwise, default to kWh
            } else {
                tariff(iInObj).convChoice = iEconConv::KWH;
                tariff(iInObj).energyConv = 0.0000002778;
                tariff(iInObj).demandConv = 0.001;
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\", Defaulting to kWh.");
            }
        } // Default conversion factors have been applied from here on

        // schedules
        // period schedule
        if (len(state.dataIPShortCut->cAlphaArgs(4)) > 0) {
            tariff(iInObj).periodSchedule = state.dataIPShortCut->cAlphaArgs(4);                          // name of the period schedule (time of day)
            tariff(iInObj).periodSchIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4)); // index to the period schedule
            if (tariff(iInObj).periodSchIndex == 0) {
                ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                ShowContinueError(state,
                                  " not found " + state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\".");
                ErrorsFound = true;
            }
        } else {
            tariff(iInObj).periodSchIndex = 0; // flag value for no schedule used
        }
        // season schedule
        if (len(state.dataIPShortCut->cAlphaArgs(5)) > 0) {
            tariff(iInObj).seasonSchedule = state.dataIPShortCut->cAlphaArgs(5); // name of the season schedule (winter/summer)
            tariff(iInObj).seasonSchIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5)); // index to the season schedule
            if (tariff(iInObj).seasonSchIndex == 0) {
                ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                ShowContinueError(state,
                                  " not found " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + state.dataIPShortCut->cAlphaArgs(5) + "\".");
                ErrorsFound = true;
            }
        } else {
            tariff(iInObj).seasonSchIndex = 0; // flag value for no schedule used
        }
        // month schedule
        if (len(state.dataIPShortCut->cAlphaArgs(6)) > 0) {
            tariff(iInObj).monthSchedule = state.dataIPShortCut->cAlphaArgs(6);                          // name of month schedule (when months end)
            tariff(iInObj).monthSchIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6)); // index to the month schedule
            if (tariff(iInObj).monthSchIndex == 0) {
                ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                ShowContinueError(state,
                                  " not found " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" + state.dataIPShortCut->cAlphaArgs(6) + "\".");
                ErrorsFound = true;
            }
        } else {
            tariff(iInObj).monthSchIndex = 0; // flag value for no schedule used
        }
        // type of demand window
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "QuarterHour")) {
            // check to make sure that the demand window and the TIMESTEP IN HOUR are consistant.
            {
                auto const SELECT_CASE_var(state.dataGlobal->NumOfTimeStepInHour);
                if ((SELECT_CASE_var == 1) || (SELECT_CASE_var == 3) || (SELECT_CASE_var == 5) || (SELECT_CASE_var == 15)) {
                    tariff(iInObj).demandWindow = iDemandWindow::Hour;
                    tariff(iInObj).demWinTime = 1.00;
                    ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                    ShowContinueError(state,
                                      format("Demand window of QuarterHour is not consistent with number of timesteps per hour [{}].",
                                             state.dataGlobal->NumOfTimeStepInHour));
                    ShowContinueError(state, "Demand window will be set to FullHour, and the simulation continues.");
                } else if ((SELECT_CASE_var == 2) || (SELECT_CASE_var == 6) || (SELECT_CASE_var == 10) || (SELECT_CASE_var == 30)) {
                    tariff(iInObj).demandWindow = iDemandWindow::Half;
                    tariff(iInObj).demWinTime = 0.50;
                    ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                    ShowContinueError(state,
                                      format("Demand window of QuarterHour is not consistent with number of timesteps per hour [{}].",
                                             state.dataGlobal->NumOfTimeStepInHour));
                    ShowContinueError(state, "Demand window will be set to HalfHour, and the simulation continues.");
                } else if ((SELECT_CASE_var == 4) || (SELECT_CASE_var == 12) || (SELECT_CASE_var == 20) || (SELECT_CASE_var == 60)) {
                    tariff(iInObj).demandWindow = iDemandWindow::Quarter;
                    tariff(iInObj).demWinTime = 0.25;
                }
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "HalfHour")) {
            {
                auto const SELECT_CASE_var(state.dataGlobal->NumOfTimeStepInHour);
                if ((SELECT_CASE_var == 1) || (SELECT_CASE_var == 3) || (SELECT_CASE_var == 5) || (SELECT_CASE_var == 15)) {
                    tariff(iInObj).demandWindow = iDemandWindow::Hour;
                    tariff(iInObj).demWinTime = 1.00;
                    ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                    ShowContinueError(state,
                                      format("Demand window of HalfHour is not consistent with number of timesteps per hour [{}].",
                                             state.dataGlobal->NumOfTimeStepInHour));
                    ShowContinueError(state, "Demand window will be set to FullHour, and the simulation continues.");
                } else if ((SELECT_CASE_var == 2) || (SELECT_CASE_var == 4) || (SELECT_CASE_var == 6) || (SELECT_CASE_var == 10) ||
                           (SELECT_CASE_var == 12) || (SELECT_CASE_var == 20) || (SELECT_CASE_var == 30) || (SELECT_CASE_var == 60)) {
                    tariff(iInObj).demandWindow = iDemandWindow::Half;
                    tariff(iInObj).demWinTime = 0.50;
                }
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "FullHour")) {
            tariff(iInObj).demandWindow = iDemandWindow::Hour;
            tariff(iInObj).demWinTime = 1.00;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "Day")) {
            tariff(iInObj).demandWindow = iDemandWindow::Day;
            tariff(iInObj).demWinTime = 24.00;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "Week")) {
            tariff(iInObj).demandWindow = iDemandWindow::Week;
            tariff(iInObj).demWinTime = 24.0 * 7.0;
        } else {
            // if not entered default to the same logic as quarter of an hour
            {
                auto const SELECT_CASE_var(state.dataGlobal->NumOfTimeStepInHour);
                if ((SELECT_CASE_var == 1) || (SELECT_CASE_var == 3) || (SELECT_CASE_var == 5) || (SELECT_CASE_var == 15)) {
                    tariff(iInObj).demandWindow = iDemandWindow::Hour;
                    tariff(iInObj).demWinTime = 1.00;
                } else if ((SELECT_CASE_var == 2) || (SELECT_CASE_var == 6) || (SELECT_CASE_var == 10) || (SELECT_CASE_var == 30)) {
                    tariff(iInObj).demandWindow = iDemandWindow::Half;
                    tariff(iInObj).demWinTime = 0.50;
                } else if ((SELECT_CASE_var == 4) || (SELECT_CASE_var == 12) || (SELECT_CASE_var == 20) || (SELECT_CASE_var == 60)) {
                    tariff(iInObj).demandWindow = iDemandWindow::Quarter;
                    tariff(iInObj).demWinTime = 0.25;
                }
            }
        }
        // monthly charge
        tariff(iInObj).monthChgVal = UtilityRoutines::ProcessNumber(state.dataIPShortCut->cAlphaArgs(8), isNotNumeric);
        tariff(iInObj).monthChgPt = AssignVariablePt(
            state, state.dataIPShortCut->cAlphaArgs(8), isNotNumeric, varIsArgument, varNotYetDefined, iEconVarObjType::Unknown, 0, iInObj);
        // minimum monthly charge
        if (len(state.dataIPShortCut->cAlphaArgs(9)) > 0) {
            tariff(iInObj).minMonthChgVal = UtilityRoutines::ProcessNumber(state.dataIPShortCut->cAlphaArgs(9), isNotNumeric);
        } else {
            tariff(iInObj).minMonthChgVal = -HUGE_(-1.0); // set to a very negative value
        }
        tariff(iInObj).minMonthChgPt = AssignVariablePt(
            state, state.dataIPShortCut->cAlphaArgs(9), isNotNumeric, varIsArgument, varNotYetDefined, iEconVarObjType::Unknown, 0, iInObj);
        // real time pricing
        tariff(iInObj).chargeSchedule = state.dataIPShortCut->cAlphaArgs(10);
        tariff(iInObj).chargeSchIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(10));
        tariff(iInObj).baseUseSchedule = state.dataIPShortCut->cAlphaArgs(11);
        tariff(iInObj).baseUseSchIndex = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(11));
        // group name for separate distribution and transmission rates
        tariff(iInObj).groupName = state.dataIPShortCut->cAlphaArgs(12);
        // buy or sell option
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(13), "BuyFromUtility")) {
            tariff(iInObj).buyOrSell = buyFromUtility;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(13), "SellToUtility")) {
            tariff(iInObj).buyOrSell = sellToUtility;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(13), "NetMetering")) {
            tariff(iInObj).buyOrSell = netMetering;
        } else {
            tariff(iInObj).buyOrSell = buyFromUtility;
        }
        // check if meter is consistent with buy or sell option
        if ((tariff(iInObj).buyOrSell == sellToUtility) &&
            (!UtilityRoutines::SameString(tariff(iInObj).reportMeter, "ELECTRICITYSURPLUSSOLD:FACILITY"))) {
            ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" atypical meter");
            ShowContinueError(state, "The meter chosen \"" + tariff(iInObj).reportMeter + "\" is not typically used with the sellToUtility option.");
            ShowContinueError(state, "Usually the ElectricitySurplusSold:Facility meter is selected when the sellToUtility option is used.");
        }
        if ((tariff(iInObj).buyOrSell == netMetering) && (!UtilityRoutines::SameString(tariff(iInObj).reportMeter, "ELECTRICITYNET:FACILITY"))) {
            ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" atypical meter");
            ShowContinueError(state, "The meter chosen \"" + tariff(iInObj).reportMeter + " is not typically used with the netMetering option.");
            ShowContinueError(state, "Usually the ElectricityNet:Facility meter is selected when the netMetering option is used.");
        }
        // also test the buy option for electricity
        if (tariff(iInObj).buyOrSell == buyFromUtility) {
            if (hasi(tariff(iInObj).reportMeter, "Elec")) { // test if electric meter
                if (!(UtilityRoutines::SameString(tariff(iInObj).reportMeter, "Electricity:Facility") ||
                      UtilityRoutines::SameString(tariff(iInObj).reportMeter, "ElectricityPurchased:Facility"))) {
                    ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" atypical meter");
                    ShowContinueError(state,
                                      "The meter chosen \"" + tariff(iInObj).reportMeter + " is not typically used with the buyFromUtility option.");
                    ShowContinueError(state,
                                      "Usually the Electricity:Facility meter or the ElectricityPurchased:Facility is selected when the "
                                      "buyFromUtility option is used.");
                }
            }
        }
        // initialize gathering arrays
        tariff(iInObj).seasonForMonth = 0;
        tariff(iInObj).gatherEnergy = 0.0;
        tariff(iInObj).gatherDemand = 0.0;
        // assume that the tariff is qualified
        tariff(iInObj).isQualified = true;
        tariff(iInObj).ptDisqualifier = 0;
        // assume that the tariff is not selected
        tariff(iInObj).isSelected = false;
        tariff(iInObj).totalAnnualCost = 0.0;
        // now create the Table Of Contents entries for an HTML file
        if (state.dataOutRptTab->displayTariffReport) {
            AddTOCEntry(state, "Tariff Report", tariff(iInObj).tariffName);
        }
        // associate the resource number with each tariff
        if (tariff(iInObj).reportMeterIndx >= 1) {
            tariff(iInObj).resourceNum = AssignResourceTypeNum(state.dataOutputProcessor->EnergyMeters(tariff(iInObj).reportMeterIndx).ResourceType);
        }
    }
}

void GetInputEconomicsQualify(EnergyPlusData &state, bool &ErrorsFound) // true if errors found during getting input objects.
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //    Read the input file for "Economics:Qualify" objects.

    std::string const RoutineName("GetInputEconomicsQualify: ");
    int iInObj;    // loop index variable for reading in objects
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool isNotNumeric;
    int jFld;
    std::string CurrentModuleObject; // for ease in renaming.

    auto &qualify(state.dataEconTariff->qualify);

    CurrentModuleObject = "UtilityCost:Qualify";
    state.dataEconTariff->numQualify = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    qualify.allocate(state.dataEconTariff->numQualify);
    for (iInObj = 1; iInObj <= state.dataEconTariff->numQualify; ++iInObj) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 iInObj,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another economic object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            if (hasi(state.dataIPShortCut->cAlphaArgs(jFld), "UtilityCost:")) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\".");
                ShowContinueError(state, "... a field was found containing UtilityCost: which may indicate a missing comma.");
            }
        }
        // index of the tariff name in the tariff array
        qualify(iInObj).tariffIndx =
            FindTariffIndex(state, state.dataIPShortCut->cAlphaArgs(2), state.dataIPShortCut->cAlphaArgs(1), ErrorsFound, CurrentModuleObject);
        warnIfNativeVarname(state, state.dataIPShortCut->cAlphaArgs(1), qualify(iInObj).tariffIndx, ErrorsFound, CurrentModuleObject);
        qualify(iInObj).namePt = AssignVariablePt(state,
                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                  true,
                                                  varIsAssigned,
                                                  varNotYetDefined,
                                                  iEconVarObjType::Qualify,
                                                  iInObj,
                                                  qualify(iInObj).tariffIndx);
        // index of the variable in the variable array
        qualify(iInObj).sourcePt = AssignVariablePt(state,
                                                    state.dataIPShortCut->cAlphaArgs(3),
                                                    true,
                                                    varIsArgument,
                                                    varNotYetDefined,
                                                    iEconVarObjType::Unknown,
                                                    0,
                                                    qualify(iInObj).tariffIndx);
        // indicator if maximum test otherwise minimum
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Minimum")) {
            qualify(iInObj).isMaximum = false;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), "Maximum")) {
            qualify(iInObj).isMaximum = true;
        } else {
            ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
            ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\".");
            ErrorsFound = true;
            qualify(iInObj).isMaximum = true;
        }
        // value of the threshold
        qualify(iInObj).thresholdVal = UtilityRoutines::ProcessNumber(state.dataIPShortCut->cAlphaArgs(5), isNotNumeric);
        qualify(iInObj).thresholdPt = AssignVariablePt(state,
                                                       state.dataIPShortCut->cAlphaArgs(5),
                                                       isNotNumeric,
                                                       varIsArgument,
                                                       varNotYetDefined,
                                                       iEconVarObjType::Unknown,
                                                       0,
                                                       qualify(iInObj).tariffIndx);
        // enumerated list of the kind of season
        qualify(iInObj).season = LookUpSeason(state, state.dataIPShortCut->cAlphaArgs(6), state.dataIPShortCut->cAlphaArgs(1));
        // indicator if consecutive months otherwise count
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "Count")) {
            qualify(iInObj).isConsecutive = false;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "Consecutive")) {
            qualify(iInObj).isConsecutive = true;
        } else {
            ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
            ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + state.dataIPShortCut->cAlphaArgs(5) + "\".");
            ErrorsFound = true;
            qualify(iInObj).isConsecutive = true;
        }
        // number of months the test must be good for
        qualify(iInObj).numberOfMonths = state.dataIPShortCut->rNumericArgs(1);
    }
}

void GetInputEconomicsChargeSimple(EnergyPlusData &state, bool &ErrorsFound) // true if errors found during getting input objects.
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //    Read the input file for "Economics:Charge:Simple" objects.

    std::string const RoutineName("GetInputEconomicsChargeSimple: ");
    int iInObj;    // loop index variable for reading in objects
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool isNotNumeric;
    int jFld;
    std::string CurrentModuleObject; // for ease in renaming.

    auto &chargeSimple(state.dataEconTariff->chargeSimple);
    auto &tariff(state.dataEconTariff->tariff);

    CurrentModuleObject = "UtilityCost:Charge:Simple";
    state.dataEconTariff->numChargeSimple = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    chargeSimple.allocate(state.dataEconTariff->numChargeSimple);
    for (iInObj = 1; iInObj <= state.dataEconTariff->numChargeSimple; ++iInObj) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 iInObj,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another economic object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            if (hasi(state.dataIPShortCut->cAlphaArgs(jFld), "UtilityCost:")) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\".");
                ShowContinueError(state, "... a field was found containing UtilityCost: which may indicate a missing comma.");
            }
        }
        // index of the tariff name in the tariff array
        chargeSimple(iInObj).tariffIndx =
            FindTariffIndex(state, state.dataIPShortCut->cAlphaArgs(2), state.dataIPShortCut->cAlphaArgs(1), ErrorsFound, CurrentModuleObject);
        warnIfNativeVarname(state, state.dataIPShortCut->cAlphaArgs(1), chargeSimple(iInObj).tariffIndx, ErrorsFound, CurrentModuleObject);
        chargeSimple(iInObj).namePt = AssignVariablePt(state,
                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                       true,
                                                       varIsAssigned,
                                                       varNotYetDefined,
                                                       iEconVarObjType::ChargeSimple,
                                                       iInObj,
                                                       chargeSimple(iInObj).tariffIndx);
        // index of the variable in the variable array
        chargeSimple(iInObj).sourcePt = AssignVariablePt(state,
                                                         state.dataIPShortCut->cAlphaArgs(3),
                                                         true,
                                                         varIsArgument,
                                                         varNotYetDefined,
                                                         iEconVarObjType::Unknown,
                                                         0,
                                                         chargeSimple(iInObj).tariffIndx);
        // enumerated list of the kind of season
        chargeSimple(iInObj).season = LookUpSeason(state, state.dataIPShortCut->cAlphaArgs(4), state.dataIPShortCut->cAlphaArgs(1));
        // check to make sure a seasonal schedule is specified if the season is not annual
        if (chargeSimple(iInObj).season != seasonAnnual) {
            if (chargeSimple(iInObj).tariffIndx != 0) {
                if (tariff(chargeSimple(iInObj).tariffIndx).seasonSchIndex == 0) {
                    ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                    ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\".");
                    ShowContinueError(state,
                                      " a Season other than Annual is used but no Season Schedule Name is specified in the UtilityCost:Tariff.");
                }
            }
        }
        // index of the category in the variable array
        chargeSimple(iInObj).categoryPt = AssignVariablePt(state,
                                                           state.dataIPShortCut->cAlphaArgs(5),
                                                           true,
                                                           varIsAssigned,
                                                           varNotYetDefined,
                                                           iEconVarObjType::Category,
                                                           iInObj,
                                                           chargeSimple(iInObj).tariffIndx);
        // cost per unit value or variable
        chargeSimple(iInObj).costPerVal = UtilityRoutines::ProcessNumber(state.dataIPShortCut->cAlphaArgs(6), isNotNumeric);
        chargeSimple(iInObj).costPerPt = AssignVariablePt(state,
                                                          state.dataIPShortCut->cAlphaArgs(6),
                                                          isNotNumeric,
                                                          varIsArgument,
                                                          varNotYetDefined,
                                                          iEconVarObjType::Unknown,
                                                          0,
                                                          chargeSimple(iInObj).tariffIndx);
    }
}

void GetInputEconomicsChargeBlock(EnergyPlusData &state, bool &ErrorsFound) // true if errors found during getting input objects.
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //    Read the input file for "Economics:Charge:Block" objects.

    std::string const RoutineName("GetInputEconomicsChargeBlock: ");
    int iInObj;    // loop index variable for reading in objects
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool isNotNumeric;
    int jBlk;               // loop index for blocks
    int alphaOffset;        // offset used in blocks for alpha array
    Real64 hugeNumber(0.0); // Autodesk Value not used but suppresses warning about HUGE_() call
    int jFld;
    std::string CurrentModuleObject; // for ease in renaming.

    auto &chargeBlock(state.dataEconTariff->chargeBlock);
    auto &tariff(state.dataEconTariff->tariff);

    CurrentModuleObject = "UtilityCost:Charge:Block";
    hugeNumber = HUGE_(hugeNumber);
    state.dataEconTariff->numChargeBlock = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    chargeBlock.allocate(state.dataEconTariff->numChargeBlock);
    for (iInObj = 1; iInObj <= state.dataEconTariff->numChargeBlock; ++iInObj) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 iInObj,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another economic object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            if (hasi(state.dataIPShortCut->cAlphaArgs(jFld), "UtilityCost:")) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\".");
                ShowContinueError(state, "... a field was found containing UtilityCost: which may indicate a missing comma.");
            }
        }
        // index of the tariff name in the tariff array
        chargeBlock(iInObj).tariffIndx =
            FindTariffIndex(state, state.dataIPShortCut->cAlphaArgs(2), state.dataIPShortCut->cAlphaArgs(1), ErrorsFound, CurrentModuleObject);
        warnIfNativeVarname(state, state.dataIPShortCut->cAlphaArgs(1), chargeBlock(iInObj).tariffIndx, ErrorsFound, CurrentModuleObject);
        chargeBlock(iInObj).namePt = AssignVariablePt(state,
                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                      true,
                                                      varIsAssigned,
                                                      varNotYetDefined,
                                                      iEconVarObjType::ChargeBlock,
                                                      iInObj,
                                                      chargeBlock(iInObj).tariffIndx);
        // index of the variable in the variable array
        chargeBlock(iInObj).sourcePt = AssignVariablePt(state,
                                                        state.dataIPShortCut->cAlphaArgs(3),
                                                        true,
                                                        varIsArgument,
                                                        varNotYetDefined,
                                                        iEconVarObjType::Unknown,
                                                        0,
                                                        chargeBlock(iInObj).tariffIndx);
        // enumerated list of the kind of season
        chargeBlock(iInObj).season = LookUpSeason(state, state.dataIPShortCut->cAlphaArgs(4), state.dataIPShortCut->cAlphaArgs(1));
        // check to make sure a seasonal schedule is specified if the season is not annual
        if (chargeBlock(iInObj).season != seasonAnnual) {
            if (chargeBlock(iInObj).tariffIndx != 0) {
                if (tariff(chargeBlock(iInObj).tariffIndx).seasonSchIndex == 0) {
                    ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
                    ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\".");
                    ShowContinueError(state,
                                      " a Season other than Annual is used but no Season Schedule Name is specified in the UtilityCost:Tariff.");
                }
            }
        }
        // index of the category in the variable array
        chargeBlock(iInObj).categoryPt = AssignVariablePt(state,
                                                          state.dataIPShortCut->cAlphaArgs(5),
                                                          true,
                                                          varIsAssigned,
                                                          varNotYetDefined,
                                                          iEconVarObjType::Category,
                                                          iInObj,
                                                          chargeBlock(iInObj).tariffIndx);
        // index of the remaining into variable in the variable array
        chargeBlock(iInObj).remainingPt = AssignVariablePt(state,
                                                           state.dataIPShortCut->cAlphaArgs(6),
                                                           true,
                                                           varIsAssigned,
                                                           varNotYetDefined,
                                                           iEconVarObjType::Category,
                                                           iInObj,
                                                           chargeBlock(iInObj).tariffIndx);
        // block size multiplier
        if (len(state.dataIPShortCut->cAlphaArgs(7)) == 0) { // if blank
            chargeBlock(iInObj).blkSzMultVal = 1.0;          // default is 1 if left blank
            chargeBlock(iInObj).blkSzMultPt = 0;
        } else {
            chargeBlock(iInObj).blkSzMultVal = UtilityRoutines::ProcessNumber(state.dataIPShortCut->cAlphaArgs(7), isNotNumeric);
            chargeBlock(iInObj).blkSzMultPt = AssignVariablePt(state,
                                                               state.dataIPShortCut->cAlphaArgs(7),
                                                               isNotNumeric,
                                                               varIsArgument,
                                                               varNotYetDefined,
                                                               iEconVarObjType::Unknown,
                                                               0,
                                                               chargeBlock(iInObj).tariffIndx);
        }
        // number of blocks used
        chargeBlock(iInObj).numBlk = (NumAlphas - 7) / 2;
        for (jBlk = 1; jBlk <= chargeBlock(iInObj).numBlk; ++jBlk) {
            alphaOffset = 7 + (jBlk - 1) * 2;
            // catch the "remaining" code word for the block size
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(alphaOffset + 1), "REMAINING")) {
                chargeBlock(iInObj).blkSzVal(jBlk) = hugeNumber / 1000000; // using small portion of largest possible value to prevent overflow
                chargeBlock(iInObj).blkSzPt(jBlk) = 0;
            } else {
                // array of block size
                chargeBlock(iInObj).blkSzVal(jBlk) = UtilityRoutines::ProcessNumber(state.dataIPShortCut->cAlphaArgs(alphaOffset + 1), isNotNumeric);

                chargeBlock(iInObj).blkSzPt(jBlk) = AssignVariablePt(state,
                                                                     state.dataIPShortCut->cAlphaArgs(alphaOffset + 1),
                                                                     isNotNumeric,
                                                                     varIsArgument,
                                                                     varNotYetDefined,
                                                                     iEconVarObjType::Unknown,
                                                                     0,
                                                                     chargeBlock(iInObj).tariffIndx);
            }
            // array of block cost
            chargeBlock(iInObj).blkCostVal(jBlk) = UtilityRoutines::ProcessNumber(state.dataIPShortCut->cAlphaArgs(alphaOffset + 2), isNotNumeric);
            chargeBlock(iInObj).blkCostPt(jBlk) = AssignVariablePt(state,
                                                                   state.dataIPShortCut->cAlphaArgs(alphaOffset + 2),
                                                                   isNotNumeric,
                                                                   varIsArgument,
                                                                   varNotYetDefined,
                                                                   iEconVarObjType::Unknown,
                                                                   0,
                                                                   chargeBlock(iInObj).tariffIndx);
        }
    }
}

void GetInputEconomicsRatchet(EnergyPlusData &state, bool &ErrorsFound) // true if errors found during getting input objects.
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //    Read the input file for "Economics:Ratchet" objects.

    std::string const RoutineName("GetInputEconomicsRatchet: ");
    int iInObj;    // loop index variable for reading in objects
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    bool isNotNumeric;
    int jFld;
    std::string CurrentModuleObject; // for ease in renaming.

    auto &ratchet(state.dataEconTariff->ratchet);

    CurrentModuleObject = "UtilityCost:Ratchet";
    state.dataEconTariff->numRatchet = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    ratchet.allocate(state.dataEconTariff->numRatchet);
    for (iInObj = 1; iInObj <= state.dataEconTariff->numRatchet; ++iInObj) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 iInObj,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another economic object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            if (hasi(state.dataIPShortCut->cAlphaArgs(jFld), "UtilityCost:")) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\".");
                ShowContinueError(state, "... a field was found containing UtilityCost: which may indicate a missing comma.");
            }
        }
        // index of the tariff name in the tariff array
        ratchet(iInObj).tariffIndx =
            FindTariffIndex(state, state.dataIPShortCut->cAlphaArgs(2), state.dataIPShortCut->cAlphaArgs(1), ErrorsFound, CurrentModuleObject);
        warnIfNativeVarname(state, state.dataIPShortCut->cAlphaArgs(1), ratchet(iInObj).tariffIndx, ErrorsFound, CurrentModuleObject);
        ratchet(iInObj).namePt = AssignVariablePt(state,
                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                  true,
                                                  varIsAssigned,
                                                  varNotYetDefined,
                                                  iEconVarObjType::Ratchet,
                                                  iInObj,
                                                  ratchet(iInObj).tariffIndx);
        // index of the variable in the variable array
        ratchet(iInObj).baselinePt = AssignVariablePt(state,
                                                      state.dataIPShortCut->cAlphaArgs(3),
                                                      true,
                                                      varIsArgument,
                                                      varNotYetDefined,
                                                      iEconVarObjType::Ratchet,
                                                      iInObj,
                                                      ratchet(iInObj).tariffIndx);
        // index of the variable in the variable array
        ratchet(iInObj).adjustmentPt = AssignVariablePt(state,
                                                        state.dataIPShortCut->cAlphaArgs(4),
                                                        true,
                                                        varIsArgument,
                                                        varNotYetDefined,
                                                        iEconVarObjType::Ratchet,
                                                        iInObj,
                                                        ratchet(iInObj).tariffIndx);
        // seasons to and from
        ratchet(iInObj).seasonFrom = LookUpSeason(state, state.dataIPShortCut->cAlphaArgs(5), state.dataIPShortCut->cAlphaArgs(1));
        ratchet(iInObj).seasonTo = LookUpSeason(state, state.dataIPShortCut->cAlphaArgs(6), state.dataIPShortCut->cAlphaArgs(1));
        // ratchet multiplier
        ratchet(iInObj).multiplierVal = UtilityRoutines::ProcessNumber(state.dataIPShortCut->cAlphaArgs(7), isNotNumeric);
        ratchet(iInObj).multiplierPt = AssignVariablePt(state,
                                                        state.dataIPShortCut->cAlphaArgs(7),
                                                        isNotNumeric,
                                                        varIsArgument,
                                                        varNotYetDefined,
                                                        iEconVarObjType::Unknown,
                                                        0,
                                                        ratchet(iInObj).tariffIndx);
        // ratchet offset
        ratchet(iInObj).offsetVal = UtilityRoutines::ProcessNumber(state.dataIPShortCut->cAlphaArgs(8), isNotNumeric);
        ratchet(iInObj).offsetPt = AssignVariablePt(state,
                                                    state.dataIPShortCut->cAlphaArgs(8),
                                                    isNotNumeric,
                                                    varIsArgument,
                                                    varNotYetDefined,
                                                    iEconVarObjType::Unknown,
                                                    0,
                                                    ratchet(iInObj).tariffIndx);
    }
}

void GetInputEconomicsVariable(EnergyPlusData &state, bool &ErrorsFound) // true if errors found during getting input objects.
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //    Read the input file for "Economics:Variable" objects.

    std::string const RoutineName("GetInputEconomicsVariable: ");

    int numEconVarObj;
    int tariffPt;
    int iInObj;    // loop index variable for reading in objects
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    int jVal;
    int variablePt;
    int jFld;
    std::string CurrentModuleObject; // for ease in renaming.

    auto &econVar(state.dataEconTariff->econVar);

    CurrentModuleObject = "UtilityCost:Variable";
    numEconVarObj = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    for (iInObj = 1; iInObj <= numEconVarObj; ++iInObj) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 iInObj,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another economic object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            if (hasi(state.dataIPShortCut->cAlphaArgs(jFld), "UtilityCost:")) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\".");
                ShowContinueError(state, "... a field was found containing UtilityCost: which may indicate a missing comma.");
            }
        }
        tariffPt = FindTariffIndex(state, state.dataIPShortCut->cAlphaArgs(2), state.dataIPShortCut->cAlphaArgs(1), ErrorsFound, CurrentModuleObject);
        variablePt = AssignVariablePt(
            state, state.dataIPShortCut->cAlphaArgs(1), true, varIsArgument, varUserDefined, iEconVarObjType::Variable, iInObj, tariffPt);
        warnIfNativeVarname(state, state.dataIPShortCut->cAlphaArgs(1), tariffPt, ErrorsFound, CurrentModuleObject);
        // validate the kind of variable - not used internally except for validation
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "ENERGY")) {
            econVar(variablePt).varUnitType = varUnitTypeEnergy;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "DEMAND")) {
            econVar(variablePt).varUnitType = varUnitTypeDemand;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "DIMENSIONLESS")) {
            econVar(variablePt).varUnitType = varUnitTypeDimensionless;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "CURRENCY")) {
            econVar(variablePt).varUnitType = varUnitTypeCurrency;
        } else {
            econVar(variablePt).varUnitType = varUnitTypeDimensionless;
            ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data");
            ShowContinueError(state, "invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\".");
            ErrorsFound = true;
        }
        // move number inputs into econVar
        for (jVal = 1; jVal <= NumNums; ++jVal) {
            econVar(variablePt).values(jVal) = state.dataIPShortCut->rNumericArgs(jVal);
        }
        // fill the rest of the array with the last value entered
        if (NumNums < MaxNumMonths) {
            for (jVal = NumNums + 1; jVal <= MaxNumMonths; ++jVal) {
                econVar(variablePt).values(jVal) = state.dataIPShortCut->rNumericArgs(NumNums);
            }
        }
    }
}

void GetInputEconomicsComputation(EnergyPlusData &state, bool &ErrorsFound) // true if errors found during getting input objects.
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //    Read the input file for "Economics:Computation" objects.
    //    This object is only used for very complex rates.

    std::string const RoutineName("GetInputEconomicsComputation: ");

    int tariffPt;
    int iInObj;    // loop index variable for reading in objects
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    int jLine;
    int jFld;
    std::string CurrentModuleObject; // for ease in renaming.

    auto &computation(state.dataEconTariff->computation);

    CurrentModuleObject = "UtilityCost:Computation";
    state.dataEconTariff->numComputation = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    computation.allocate(state.dataEconTariff->numTariff); // not the number of Computations but the number of tariffs
    // set default values for computation
    for (auto &e : computation) {
        e.computeName.clear();
        e.firstStep = 0;
        e.lastStep = -1;
        e.isUserDef = false;
    }
    for (iInObj = 1; iInObj <= state.dataEconTariff->numComputation; ++iInObj) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 iInObj,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another economic object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            if (hasi(state.dataIPShortCut->cAlphaArgs(jFld), "UtilityCost:")) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\".");
                ShowContinueError(state, "... a field was found containing UtilityCost: which may indicate a missing comma.");
            }
        }
        tariffPt = FindTariffIndex(state, state.dataIPShortCut->cAlphaArgs(2), state.dataIPShortCut->cAlphaArgs(1), ErrorsFound, CurrentModuleObject);
        warnIfNativeVarname(state, state.dataIPShortCut->cAlphaArgs(1), tariffPt, ErrorsFound, CurrentModuleObject);
        // tariff and computation share the same index, the tariff index
        // so all references are to the tariffPt
        if (isWithinRange(state, tariffPt, 1, state.dataEconTariff->numTariff)) {
            computation(tariffPt).computeName = state.dataIPShortCut->cAlphaArgs(1);
            computation(tariffPt).firstStep = state.dataEconTariff->numSteps + 1;
            for (jLine = 3; jLine <= NumAlphas; ++jLine) {
                parseComputeLine(state, state.dataIPShortCut->cAlphaArgs(jLine), tariffPt);
            }
            computation(tariffPt).lastStep = state.dataEconTariff->numSteps;
            // check to make sure that some steps were defined
            if (computation(tariffPt).firstStep >= computation(tariffPt).lastStep) {
                computation(tariffPt).firstStep = 0;
                computation(tariffPt).lastStep = -1;
                computation(tariffPt).isUserDef = false;
                ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data.");
                ShowContinueError(state, "... No lines in the computation can be interpreted ");
                ErrorsFound = true;
            } else {
                computation(tariffPt).isUserDef = true;
            }
        } else {
            ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data.");
            ShowContinueError(state,
                              "... not found " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\".");
            ErrorsFound = true;
        }
    }
}

void GetInputEconomicsCurrencyType(EnergyPlusData &state, bool &ErrorsFound) // true if errors found during getting input objects.
{
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   August 2008

    //   Sets the type of currency (U.S. Dollar, Euro, Yen, etc.. )
    //   This is a "unique" object.

    std::string const CurrentModuleObject("CurrencyType");
    std::string const RoutineName("GetInputEconomicsCurrencyType: ");

    int NumCurrencyType;
    int NumAlphas; // Number of elements in the alpha array
    int NumNums;   // Number of elements in the numeric array
    int IOStat;    // IO Status when calling get input subroutine
    int i;

    initializeMonetaryUnit(state);
    NumCurrencyType = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataCostEstimateManager->selectedMonetaryUnit = 0; // invalid
    if (NumCurrencyType == 0) {
        state.dataCostEstimateManager->selectedMonetaryUnit = 1; // USD - U.S. Dollar
    } else if (NumCurrencyType == 1) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 1,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // Monetary Unit
        for (i = 1; i <= state.dataCostEstimateManager->numMonetaryUnit; ++i) {
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1), state.dataCostEstimateManager->monetaryUnit(i).code)) {
                state.dataCostEstimateManager->selectedMonetaryUnit = i;
                break;
            }
        }
        if (state.dataCostEstimateManager->selectedMonetaryUnit == 0) {
            ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid data.");
            ShowContinueError(state, "... invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + '.');
            ErrorsFound = true;
        }
    } else if (NumCurrencyType > 1) {
        ShowWarningError(state, RoutineName + CurrentModuleObject + " Only one instance of this object is allowed. USD will be used.");
        state.dataCostEstimateManager->selectedMonetaryUnit = 1; // USD - U.S. Dollar
    }
}

void parseComputeLine(EnergyPlusData &state, std::string const &lineOfCompute, int const fromTariff)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   June 2004

    //   Converts a single line in the ECONOMICS:COMPUTE
    //   command into tokens for computation

    //   Scan the line from the end of the line to the front of the
    //   line and search for operators and variables. All items
    //   are put into the step array.

    std::string word;
    std::string::size_type endOfWord;
    int token;

    endOfWord = len(lineOfCompute) - 1;
    while (endOfWord != std::string::npos) {
        // get a single word (text string delimited by spaces)
        GetLastWord(lineOfCompute, endOfWord, word);
        // first see if word is an operator
        token = lookupOperator(word);
        // if not an operator then look for
        if (token == 0) {
            // see if argument or assignment (assignment will be first string on line)
            if (endOfWord != std::string::npos) {
                token = AssignVariablePt(state, word, true, varIsArgument, varNotYetDefined, iEconVarObjType::Unknown, 0, fromTariff);
            } else {
                token = AssignVariablePt(state, word, true, varIsAssigned, varNotYetDefined, iEconVarObjType::AssignCompute, 0, fromTariff);
            }
        }
        // if a token is found then put it into step array
        if (token == 0) {
            ShowWarningError(state, "In UtilityCost:Computation line: " + lineOfCompute);
            ShowContinueError(state, "  Do not recognize: " + word + " Will skip.");
        } else {
            incrementSteps(state);
            state.dataEconTariff->steps(state.dataEconTariff->numSteps) = token;
        }
    }
    incrementSteps(state);
    state.dataEconTariff->steps(state.dataEconTariff->numSteps) = 0; // at the end of the line show a zero to clear the stack
}

void GetLastWord(std::string const &lineOfText, std::string::size_type &endOfScan, std::string &aWord)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   June 2004

    //   Returns the last substring of the line of text to the
    //   left of the endOfSubStrg pointer. A substring is
    //   delimitted by spaces.  Quotes are not significant
    //   (they are treated just like any other non-space character)

    //   Scan the string from the end.

    bool isInWord;
    bool isSpace;
    std::string::size_type iString;
    std::string::size_type curEndOfScan;
    std::string::size_type beginOfWord;
    std::string::size_type endOfWord;

    curEndOfScan = endOfScan;
    if (curEndOfScan != std::string::npos) {
        if (curEndOfScan >= len(lineOfText)) {
            curEndOfScan = len(lineOfText) - 1;
        }
        // check if currently on a space or not
        if (lineOfText[curEndOfScan] == ' ') {
            isInWord = false;
            beginOfWord = 0;
            endOfWord = 0;
        } else {
            isInWord = true;
            beginOfWord = curEndOfScan;
            endOfWord = curEndOfScan;
        }
        // scan backwards from
        for (iString = curEndOfScan; iString <= curEndOfScan; --iString) { // Unsigned will wrap to npos after 0
            if (lineOfText[iString] == ' ') {
                isSpace = true;
            } else {
                isSpace = false;
            }
            // all logical conditions of isSpace and isInWord
            if (isSpace) {
                if (isInWord) {
                    // found the space in front of the word
                    break;
                } else {
                    // still have not found the back of the word
                    // do nothing
                }
            } else {
                if (isInWord) {
                    // still have not found the space in front of the word
                    beginOfWord = iString;
                } else {
                    // found the last character of the word
                    endOfWord = iString;
                    beginOfWord = iString;
                    isInWord = true;
                }
            }
        }
        aWord = lineOfText.substr(beginOfWord, endOfWord - beginOfWord + 1);
        endOfScan = beginOfWord - 1;
        if (endOfScan == std::string::npos) {
            endOfScan = std::string::npos;
        }
    } else {
        endOfScan = std::string::npos;
        aWord = "";
    }
}

void initializeMonetaryUnit(EnergyPlusData &state)
{
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   August 2008

    //   Sets the type of monetary unit array.

    //   Uses get input structure similar to other objects
    //   The monetaryUnitSymbols.xls spreadsheet helps create the code for this routine

    //   www.xe.com/symbols.php

    state.dataCostEstimateManager->numMonetaryUnit = 111;
    state.dataCostEstimateManager->monetaryUnit.allocate(state.dataCostEstimateManager->numMonetaryUnit);
    state.dataCostEstimateManager->monetaryUnit(1).code = "USD";
    state.dataCostEstimateManager->monetaryUnit(2).code = "AFN";
    state.dataCostEstimateManager->monetaryUnit(3).code = "ALL";
    state.dataCostEstimateManager->monetaryUnit(4).code = "ANG";
    state.dataCostEstimateManager->monetaryUnit(5).code = "ARS";
    state.dataCostEstimateManager->monetaryUnit(6).code = "AUD";
    state.dataCostEstimateManager->monetaryUnit(7).code = "AWG";
    state.dataCostEstimateManager->monetaryUnit(8).code = "AZN";
    state.dataCostEstimateManager->monetaryUnit(9).code = "BAM";
    state.dataCostEstimateManager->monetaryUnit(10).code = "BBD";
    state.dataCostEstimateManager->monetaryUnit(11).code = "BGN";
    state.dataCostEstimateManager->monetaryUnit(12).code = "BMD";
    state.dataCostEstimateManager->monetaryUnit(13).code = "BND";
    state.dataCostEstimateManager->monetaryUnit(14).code = "BOB";
    state.dataCostEstimateManager->monetaryUnit(15).code = "BRL";
    state.dataCostEstimateManager->monetaryUnit(16).code = "BSD";
    state.dataCostEstimateManager->monetaryUnit(17).code = "BWP";
    state.dataCostEstimateManager->monetaryUnit(18).code = "BYR";
    state.dataCostEstimateManager->monetaryUnit(19).code = "BZD";
    state.dataCostEstimateManager->monetaryUnit(20).code = "CAD";
    state.dataCostEstimateManager->monetaryUnit(21).code = "CHF";
    state.dataCostEstimateManager->monetaryUnit(22).code = "CLP";
    state.dataCostEstimateManager->monetaryUnit(23).code = "CNY";
    state.dataCostEstimateManager->monetaryUnit(24).code = "COP";
    state.dataCostEstimateManager->monetaryUnit(25).code = "CRC";
    state.dataCostEstimateManager->monetaryUnit(26).code = "CUP";
    state.dataCostEstimateManager->monetaryUnit(27).code = "CZK";
    state.dataCostEstimateManager->monetaryUnit(28).code = "DKK";
    state.dataCostEstimateManager->monetaryUnit(29).code = "DOP";
    state.dataCostEstimateManager->monetaryUnit(30).code = "EEK";
    state.dataCostEstimateManager->monetaryUnit(31).code = "EGP";
    state.dataCostEstimateManager->monetaryUnit(32).code = "EUR";
    state.dataCostEstimateManager->monetaryUnit(33).code = "FJD";
    state.dataCostEstimateManager->monetaryUnit(34).code = "GBP";
    state.dataCostEstimateManager->monetaryUnit(35).code = "GHC";
    state.dataCostEstimateManager->monetaryUnit(36).code = "GIP";
    state.dataCostEstimateManager->monetaryUnit(37).code = "GTQ";
    state.dataCostEstimateManager->monetaryUnit(38).code = "GYD";
    state.dataCostEstimateManager->monetaryUnit(39).code = "HKD";
    state.dataCostEstimateManager->monetaryUnit(40).code = "HNL";
    state.dataCostEstimateManager->monetaryUnit(41).code = "HRK";
    state.dataCostEstimateManager->monetaryUnit(42).code = "HUF";
    state.dataCostEstimateManager->monetaryUnit(43).code = "IDR";
    state.dataCostEstimateManager->monetaryUnit(44).code = "ILS";
    state.dataCostEstimateManager->monetaryUnit(45).code = "IMP";
    state.dataCostEstimateManager->monetaryUnit(46).code = "INR";
    state.dataCostEstimateManager->monetaryUnit(47).code = "IRR";
    state.dataCostEstimateManager->monetaryUnit(48).code = "ISK";
    state.dataCostEstimateManager->monetaryUnit(49).code = "JEP";
    state.dataCostEstimateManager->monetaryUnit(50).code = "JMD";
    state.dataCostEstimateManager->monetaryUnit(51).code = "JPY";
    state.dataCostEstimateManager->monetaryUnit(52).code = "KGS";
    state.dataCostEstimateManager->monetaryUnit(53).code = "KHR";
    state.dataCostEstimateManager->monetaryUnit(54).code = "KPW";
    state.dataCostEstimateManager->monetaryUnit(55).code = "KRW";
    state.dataCostEstimateManager->monetaryUnit(56).code = "KYD";
    state.dataCostEstimateManager->monetaryUnit(57).code = "KZT";
    state.dataCostEstimateManager->monetaryUnit(58).code = "LAK";
    state.dataCostEstimateManager->monetaryUnit(59).code = "LBP";
    state.dataCostEstimateManager->monetaryUnit(60).code = "LKR";
    state.dataCostEstimateManager->monetaryUnit(61).code = "LRD";
    state.dataCostEstimateManager->monetaryUnit(62).code = "LTL";
    state.dataCostEstimateManager->monetaryUnit(63).code = "LVL";
    state.dataCostEstimateManager->monetaryUnit(64).code = "MKD";
    state.dataCostEstimateManager->monetaryUnit(65).code = "MNT";
    state.dataCostEstimateManager->monetaryUnit(66).code = "MUR";
    state.dataCostEstimateManager->monetaryUnit(67).code = "MXN";
    state.dataCostEstimateManager->monetaryUnit(68).code = "MYR";
    state.dataCostEstimateManager->monetaryUnit(69).code = "MZN";
    state.dataCostEstimateManager->monetaryUnit(70).code = "NAD";
    state.dataCostEstimateManager->monetaryUnit(71).code = "NGN";
    state.dataCostEstimateManager->monetaryUnit(72).code = "NIO";
    state.dataCostEstimateManager->monetaryUnit(73).code = "NOK";
    state.dataCostEstimateManager->monetaryUnit(74).code = "NPR";
    state.dataCostEstimateManager->monetaryUnit(75).code = "NZD";
    state.dataCostEstimateManager->monetaryUnit(76).code = "OMR";
    state.dataCostEstimateManager->monetaryUnit(77).code = "PAB";
    state.dataCostEstimateManager->monetaryUnit(78).code = "PEN";
    state.dataCostEstimateManager->monetaryUnit(79).code = "PHP";
    state.dataCostEstimateManager->monetaryUnit(80).code = "PKR";
    state.dataCostEstimateManager->monetaryUnit(81).code = "PLN";
    state.dataCostEstimateManager->monetaryUnit(82).code = "PYG";
    state.dataCostEstimateManager->monetaryUnit(83).code = "QAR";
    state.dataCostEstimateManager->monetaryUnit(84).code = "RON";
    state.dataCostEstimateManager->monetaryUnit(85).code = "RSD";
    state.dataCostEstimateManager->monetaryUnit(86).code = "RUB";
    state.dataCostEstimateManager->monetaryUnit(87).code = "SAR";
    state.dataCostEstimateManager->monetaryUnit(88).code = "SBD";
    state.dataCostEstimateManager->monetaryUnit(89).code = "SCR";
    state.dataCostEstimateManager->monetaryUnit(90).code = "SEK";
    state.dataCostEstimateManager->monetaryUnit(91).code = "SGD";
    state.dataCostEstimateManager->monetaryUnit(92).code = "SHP";
    state.dataCostEstimateManager->monetaryUnit(93).code = "SOS";
    state.dataCostEstimateManager->monetaryUnit(94).code = "SRD";
    state.dataCostEstimateManager->monetaryUnit(95).code = "SVC";
    state.dataCostEstimateManager->monetaryUnit(96).code = "SYP";
    state.dataCostEstimateManager->monetaryUnit(97).code = "THB";
    state.dataCostEstimateManager->monetaryUnit(98).code = "TRL";
    state.dataCostEstimateManager->monetaryUnit(99).code = "TRY";
    state.dataCostEstimateManager->monetaryUnit(100).code = "TTD";
    state.dataCostEstimateManager->monetaryUnit(101).code = "TVD";
    state.dataCostEstimateManager->monetaryUnit(102).code = "TWD";
    state.dataCostEstimateManager->monetaryUnit(103).code = "UAH";
    state.dataCostEstimateManager->monetaryUnit(104).code = "UYU";
    state.dataCostEstimateManager->monetaryUnit(105).code = "UZS";
    state.dataCostEstimateManager->monetaryUnit(106).code = "VEF";
    state.dataCostEstimateManager->monetaryUnit(107).code = "VND";
    state.dataCostEstimateManager->monetaryUnit(108).code = "XCD";
    state.dataCostEstimateManager->monetaryUnit(109).code = "YER";
    state.dataCostEstimateManager->monetaryUnit(110).code = "ZAR";
    state.dataCostEstimateManager->monetaryUnit(111).code = "ZWD";

    state.dataCostEstimateManager->monetaryUnit(1).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(2).txt = "AFN";
    state.dataCostEstimateManager->monetaryUnit(3).txt = "Lek";
    state.dataCostEstimateManager->monetaryUnit(4).txt = "ANG";
    state.dataCostEstimateManager->monetaryUnit(5).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(6).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(7).txt = "AWG";
    state.dataCostEstimateManager->monetaryUnit(8).txt = "AZN";
    state.dataCostEstimateManager->monetaryUnit(9).txt = "KM";
    state.dataCostEstimateManager->monetaryUnit(10).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(11).txt = "BGN";
    state.dataCostEstimateManager->monetaryUnit(12).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(13).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(14).txt = "$b";
    state.dataCostEstimateManager->monetaryUnit(15).txt = "R$";
    state.dataCostEstimateManager->monetaryUnit(16).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(17).txt = "P";
    state.dataCostEstimateManager->monetaryUnit(18).txt = "p.";
    state.dataCostEstimateManager->monetaryUnit(19).txt = "BZ$";
    state.dataCostEstimateManager->monetaryUnit(20).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(21).txt = "CHF";
    state.dataCostEstimateManager->monetaryUnit(22).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(23).txt = "CNY";
    state.dataCostEstimateManager->monetaryUnit(24).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(25).txt = "CRC";
    state.dataCostEstimateManager->monetaryUnit(26).txt = "CUP";
    state.dataCostEstimateManager->monetaryUnit(27).txt = "CZK";
    state.dataCostEstimateManager->monetaryUnit(28).txt = "kr";
    state.dataCostEstimateManager->monetaryUnit(29).txt = "RD$";
    state.dataCostEstimateManager->monetaryUnit(30).txt = "kr";
    state.dataCostEstimateManager->monetaryUnit(31).txt = "£";
    state.dataCostEstimateManager->monetaryUnit(32).txt = "EUR";
    state.dataCostEstimateManager->monetaryUnit(33).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(34).txt = "£";
    state.dataCostEstimateManager->monetaryUnit(35).txt = "¢";
    state.dataCostEstimateManager->monetaryUnit(36).txt = "£";
    state.dataCostEstimateManager->monetaryUnit(37).txt = "Q";
    state.dataCostEstimateManager->monetaryUnit(38).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(39).txt = "HK$";
    state.dataCostEstimateManager->monetaryUnit(40).txt = "L";
    state.dataCostEstimateManager->monetaryUnit(41).txt = "kn";
    state.dataCostEstimateManager->monetaryUnit(42).txt = "Ft";
    state.dataCostEstimateManager->monetaryUnit(43).txt = "Rp";
    state.dataCostEstimateManager->monetaryUnit(44).txt = "ILS";
    state.dataCostEstimateManager->monetaryUnit(45).txt = "£";
    state.dataCostEstimateManager->monetaryUnit(46).txt = "INR";
    state.dataCostEstimateManager->monetaryUnit(47).txt = "IRR";
    state.dataCostEstimateManager->monetaryUnit(48).txt = "kr";
    state.dataCostEstimateManager->monetaryUnit(49).txt = "£";
    state.dataCostEstimateManager->monetaryUnit(50).txt = "J$";
    state.dataCostEstimateManager->monetaryUnit(51).txt = "¥";
    state.dataCostEstimateManager->monetaryUnit(52).txt = "KGS";
    state.dataCostEstimateManager->monetaryUnit(53).txt = "KHR";
    state.dataCostEstimateManager->monetaryUnit(54).txt = "KPW";
    state.dataCostEstimateManager->monetaryUnit(55).txt = "KRW";
    state.dataCostEstimateManager->monetaryUnit(56).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(57).txt = "KZT";
    state.dataCostEstimateManager->monetaryUnit(58).txt = "LAK";
    state.dataCostEstimateManager->monetaryUnit(59).txt = "£";
    state.dataCostEstimateManager->monetaryUnit(60).txt = "LKR";
    state.dataCostEstimateManager->monetaryUnit(61).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(62).txt = "Lt";
    state.dataCostEstimateManager->monetaryUnit(63).txt = "Ls";
    state.dataCostEstimateManager->monetaryUnit(64).txt = "MKD";
    state.dataCostEstimateManager->monetaryUnit(65).txt = "MNT";
    state.dataCostEstimateManager->monetaryUnit(66).txt = "MUR";
    state.dataCostEstimateManager->monetaryUnit(67).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(68).txt = "RM";
    state.dataCostEstimateManager->monetaryUnit(69).txt = "MT";
    state.dataCostEstimateManager->monetaryUnit(70).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(71).txt = "NGN";
    state.dataCostEstimateManager->monetaryUnit(72).txt = "C$";
    state.dataCostEstimateManager->monetaryUnit(73).txt = "kr";
    state.dataCostEstimateManager->monetaryUnit(74).txt = "NPR";
    state.dataCostEstimateManager->monetaryUnit(75).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(76).txt = "OMR";
    state.dataCostEstimateManager->monetaryUnit(77).txt = "B/.";
    state.dataCostEstimateManager->monetaryUnit(78).txt = "S/.";
    state.dataCostEstimateManager->monetaryUnit(79).txt = "Php";
    state.dataCostEstimateManager->monetaryUnit(80).txt = "PKR";
    state.dataCostEstimateManager->monetaryUnit(81).txt = "PLN";
    state.dataCostEstimateManager->monetaryUnit(82).txt = "Gs";
    state.dataCostEstimateManager->monetaryUnit(83).txt = "QAR";
    state.dataCostEstimateManager->monetaryUnit(84).txt = "lei";
    state.dataCostEstimateManager->monetaryUnit(85).txt = "RSD";
    state.dataCostEstimateManager->monetaryUnit(86).txt = "RUB";
    state.dataCostEstimateManager->monetaryUnit(87).txt = "SAR";
    state.dataCostEstimateManager->monetaryUnit(88).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(89).txt = "SCR";
    state.dataCostEstimateManager->monetaryUnit(90).txt = "kr";
    state.dataCostEstimateManager->monetaryUnit(91).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(92).txt = "£";
    state.dataCostEstimateManager->monetaryUnit(93).txt = "S";
    state.dataCostEstimateManager->monetaryUnit(94).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(95).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(96).txt = "£";
    state.dataCostEstimateManager->monetaryUnit(97).txt = "THB";
    state.dataCostEstimateManager->monetaryUnit(98).txt = "TRL";
    state.dataCostEstimateManager->monetaryUnit(99).txt = "YTL";
    state.dataCostEstimateManager->monetaryUnit(100).txt = "TT$";
    state.dataCostEstimateManager->monetaryUnit(101).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(102).txt = "NT$";
    state.dataCostEstimateManager->monetaryUnit(103).txt = "UAH";
    state.dataCostEstimateManager->monetaryUnit(104).txt = "$U";
    state.dataCostEstimateManager->monetaryUnit(105).txt = "UZS";
    state.dataCostEstimateManager->monetaryUnit(106).txt = "Bs";
    state.dataCostEstimateManager->monetaryUnit(107).txt = "VND";
    state.dataCostEstimateManager->monetaryUnit(108).txt = "$";
    state.dataCostEstimateManager->monetaryUnit(109).txt = "YER";
    state.dataCostEstimateManager->monetaryUnit(110).txt = "R";
    state.dataCostEstimateManager->monetaryUnit(111).txt = "Z$";

    state.dataCostEstimateManager->monetaryUnit(1).html = "$";
    state.dataCostEstimateManager->monetaryUnit(2).html = "&#x060b;";
    state.dataCostEstimateManager->monetaryUnit(3).html = "Lek";
    state.dataCostEstimateManager->monetaryUnit(4).html = "&#x0192;";
    state.dataCostEstimateManager->monetaryUnit(5).html = "$";
    state.dataCostEstimateManager->monetaryUnit(6).html = "$";
    state.dataCostEstimateManager->monetaryUnit(7).html = "&#x0192;";
    state.dataCostEstimateManager->monetaryUnit(8).html = "&#x043c;&#x0430;&#x043d;";
    state.dataCostEstimateManager->monetaryUnit(9).html = "KM";
    state.dataCostEstimateManager->monetaryUnit(10).html = "$";
    state.dataCostEstimateManager->monetaryUnit(11).html = "&#x043b;&#x0432;";
    state.dataCostEstimateManager->monetaryUnit(12).html = "$";
    state.dataCostEstimateManager->monetaryUnit(13).html = "$";
    state.dataCostEstimateManager->monetaryUnit(14).html = "$b";
    state.dataCostEstimateManager->monetaryUnit(15).html = "R$";
    state.dataCostEstimateManager->monetaryUnit(16).html = "$";
    state.dataCostEstimateManager->monetaryUnit(17).html = "P";
    state.dataCostEstimateManager->monetaryUnit(18).html = "p.";
    state.dataCostEstimateManager->monetaryUnit(19).html = "BZ$";
    state.dataCostEstimateManager->monetaryUnit(20).html = "$";
    state.dataCostEstimateManager->monetaryUnit(21).html = "CHF";
    state.dataCostEstimateManager->monetaryUnit(22).html = "$";
    state.dataCostEstimateManager->monetaryUnit(23).html = "&#x5143;";
    state.dataCostEstimateManager->monetaryUnit(24).html = "$";
    state.dataCostEstimateManager->monetaryUnit(25).html = "&#x20a1;";
    state.dataCostEstimateManager->monetaryUnit(26).html = "&#x20b1;";
    state.dataCostEstimateManager->monetaryUnit(27).html = "&#x004b;&#x010d;";
    state.dataCostEstimateManager->monetaryUnit(28).html = "kr";
    state.dataCostEstimateManager->monetaryUnit(29).html = "RD$";
    state.dataCostEstimateManager->monetaryUnit(30).html = "kr";
    state.dataCostEstimateManager->monetaryUnit(31).html = "£";
    state.dataCostEstimateManager->monetaryUnit(32).html = "&#x20ac;";
    state.dataCostEstimateManager->monetaryUnit(33).html = "$";
    state.dataCostEstimateManager->monetaryUnit(34).html = "£";
    state.dataCostEstimateManager->monetaryUnit(35).html = "¢";
    state.dataCostEstimateManager->monetaryUnit(36).html = "£";
    state.dataCostEstimateManager->monetaryUnit(37).html = "Q";
    state.dataCostEstimateManager->monetaryUnit(38).html = "$";
    state.dataCostEstimateManager->monetaryUnit(39).html = "HK$";
    state.dataCostEstimateManager->monetaryUnit(40).html = "L";
    state.dataCostEstimateManager->monetaryUnit(41).html = "kn";
    state.dataCostEstimateManager->monetaryUnit(42).html = "Ft";
    state.dataCostEstimateManager->monetaryUnit(43).html = "Rp";
    state.dataCostEstimateManager->monetaryUnit(44).html = "&#x20aa;";
    state.dataCostEstimateManager->monetaryUnit(45).html = "£";
    state.dataCostEstimateManager->monetaryUnit(46).html = "&#x20a8;";
    state.dataCostEstimateManager->monetaryUnit(47).html = "&#xfdfc;";
    state.dataCostEstimateManager->monetaryUnit(48).html = "kr";
    state.dataCostEstimateManager->monetaryUnit(49).html = "£";
    state.dataCostEstimateManager->monetaryUnit(50).html = "J$";
    state.dataCostEstimateManager->monetaryUnit(51).html = "¥";
    state.dataCostEstimateManager->monetaryUnit(52).html = "&#x043b;&#x0432;";
    state.dataCostEstimateManager->monetaryUnit(53).html = "&#x17db;";
    state.dataCostEstimateManager->monetaryUnit(54).html = "&#x20a9;";
    state.dataCostEstimateManager->monetaryUnit(55).html = "&#x20a9;";
    state.dataCostEstimateManager->monetaryUnit(56).html = "$";
    state.dataCostEstimateManager->monetaryUnit(57).html = "&#x043b;&#x0432;";
    state.dataCostEstimateManager->monetaryUnit(58).html = "&#x20ad;";
    state.dataCostEstimateManager->monetaryUnit(59).html = "£";
    state.dataCostEstimateManager->monetaryUnit(60).html = "&#x20a8;";
    state.dataCostEstimateManager->monetaryUnit(61).html = "$";
    state.dataCostEstimateManager->monetaryUnit(62).html = "Lt";
    state.dataCostEstimateManager->monetaryUnit(63).html = "Ls";
    state.dataCostEstimateManager->monetaryUnit(64).html = "&#x0434;&#x0435;&#x043d;";
    state.dataCostEstimateManager->monetaryUnit(65).html = "&#x20ae;";
    state.dataCostEstimateManager->monetaryUnit(66).html = "&#x20a8;";
    state.dataCostEstimateManager->monetaryUnit(67).html = "$";
    state.dataCostEstimateManager->monetaryUnit(68).html = "RM";
    state.dataCostEstimateManager->monetaryUnit(69).html = "MT";
    state.dataCostEstimateManager->monetaryUnit(70).html = "$";
    state.dataCostEstimateManager->monetaryUnit(71).html = "&#x20a6;";
    state.dataCostEstimateManager->monetaryUnit(72).html = "C$";
    state.dataCostEstimateManager->monetaryUnit(73).html = "kr";
    state.dataCostEstimateManager->monetaryUnit(74).html = "&#x20a8;";
    state.dataCostEstimateManager->monetaryUnit(75).html = "$";
    state.dataCostEstimateManager->monetaryUnit(76).html = "&#xfdfc;";
    state.dataCostEstimateManager->monetaryUnit(77).html = "B/.";
    state.dataCostEstimateManager->monetaryUnit(78).html = "S/.";
    state.dataCostEstimateManager->monetaryUnit(79).html = "Php";
    state.dataCostEstimateManager->monetaryUnit(80).html = "&#x20a8;";
    state.dataCostEstimateManager->monetaryUnit(81).html = "&#x007a;&#x0142;";
    state.dataCostEstimateManager->monetaryUnit(82).html = "Gs";
    state.dataCostEstimateManager->monetaryUnit(83).html = "&#xfdfc;";
    state.dataCostEstimateManager->monetaryUnit(84).html = "lei";
    state.dataCostEstimateManager->monetaryUnit(85).html = "&#x0414;&#x0438;&#x043d;&#x002e;";
    state.dataCostEstimateManager->monetaryUnit(86).html = "&#x0440;&#x0443;&#x0431;";
    state.dataCostEstimateManager->monetaryUnit(87).html = "&#xfdfc;";
    state.dataCostEstimateManager->monetaryUnit(88).html = "$";
    state.dataCostEstimateManager->monetaryUnit(89).html = "&#x20a8;";
    state.dataCostEstimateManager->monetaryUnit(90).html = "kr";
    state.dataCostEstimateManager->monetaryUnit(91).html = "$";
    state.dataCostEstimateManager->monetaryUnit(92).html = "£";
    state.dataCostEstimateManager->monetaryUnit(93).html = "S";
    state.dataCostEstimateManager->monetaryUnit(94).html = "$";
    state.dataCostEstimateManager->monetaryUnit(95).html = "$";
    state.dataCostEstimateManager->monetaryUnit(96).html = "£";
    state.dataCostEstimateManager->monetaryUnit(97).html = "&#x0e3f;";
    state.dataCostEstimateManager->monetaryUnit(98).html = "&#x20a4;";
    state.dataCostEstimateManager->monetaryUnit(99).html = "YTL";
    state.dataCostEstimateManager->monetaryUnit(100).html = "TT$";
    state.dataCostEstimateManager->monetaryUnit(101).html = "$";
    state.dataCostEstimateManager->monetaryUnit(102).html = "NT$";
    state.dataCostEstimateManager->monetaryUnit(103).html = "&#x20b4;";
    state.dataCostEstimateManager->monetaryUnit(104).html = "$U";
    state.dataCostEstimateManager->monetaryUnit(105).html = "&#x043b;&#x0432;";
    state.dataCostEstimateManager->monetaryUnit(106).html = "Bs";
    state.dataCostEstimateManager->monetaryUnit(107).html = "&#x20ab;";
    state.dataCostEstimateManager->monetaryUnit(108).html = "$";
    state.dataCostEstimateManager->monetaryUnit(109).html = "&#xfdfc;";
    state.dataCostEstimateManager->monetaryUnit(110).html = "R";
    state.dataCostEstimateManager->monetaryUnit(111).html = "Z$";
}

int LookUpSeason(EnergyPlusData &state, std::string const &nameOfSeason, std::string const &nameOfReferingObj)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //    Find the index for the season string provided or else
    //    raise a warning.

    int LookUpSeason;

    if (UtilityRoutines::SameString(nameOfSeason, "Summer")) {
        LookUpSeason = seasonSummer;
    } else if (UtilityRoutines::SameString(nameOfSeason, "Winter")) {
        LookUpSeason = seasonWinter;
    } else if (UtilityRoutines::SameString(nameOfSeason, "Spring")) {
        LookUpSeason = seasonSpring;
    } else if (UtilityRoutines::SameString(nameOfSeason, "Fall")) {
        LookUpSeason = seasonFall;
    } else if (UtilityRoutines::SameString(nameOfSeason, "Annual")) {
        LookUpSeason = seasonAnnual;
    } else {
        ShowWarningError(state, "UtilityCost: Invalid season name " + nameOfSeason + " in: " + nameOfReferingObj);
        ShowContinueError(state, "  Defaulting to Annual");
        LookUpSeason = seasonAnnual;
    }
    return LookUpSeason;
}

int FindTariffIndex(
    EnergyPlusData &state, std::string const &nameOfTariff, std::string const &nameOfReferingObj, bool &ErrorsFound, std::string const &nameOfCurObj)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //    Find the index for the tariff string provided or else
    //    raise a warning.

    int FindTariffIndex;
    int iTariff;
    int found;

    found = 0;
    for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
        if (UtilityRoutines::SameString(nameOfTariff, state.dataEconTariff->tariff(iTariff).tariffName)) {
            found = iTariff;
            break;
        }
    }
    if (found > 0) {
        FindTariffIndex = found;
    } else {
        ShowSevereError(state, nameOfCurObj + "=\"" + nameOfReferingObj + "\" invalid tariff referenced");
        ShowContinueError(state, "not found UtilityCost:Tariff=\"" + nameOfTariff + "\".");
        ErrorsFound = true;
        FindTariffIndex = 0;
    }
    return FindTariffIndex;
}

void warnIfNativeVarname(
    EnergyPlusData &state, std::string const &objName, int const curTariffIndex, bool &ErrorsFound, std::string const &curobjName)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   March 2007

    //   Issue a warning if the variable name (usually the object name) is
    //   one of the names of native variables

    bool throwError;

    throwError = false;
    if (UtilityRoutines::SameString(objName, "TotalEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "TotalDemand")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakDemand")) throwError = true;
    if (UtilityRoutines::SameString(objName, "ShoulderEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "ShoulderDemand")) throwError = true;
    if (UtilityRoutines::SameString(objName, "OffPeakEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "OffPeakDemand")) throwError = true;
    if (UtilityRoutines::SameString(objName, "MidPeakEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "MidPeakDemand")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakExceedsOffPeak")) throwError = true;
    if (UtilityRoutines::SameString(objName, "OffPeakExceedsPeak")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakExceedsMidPeak")) throwError = true;
    if (UtilityRoutines::SameString(objName, "MidPeakExceedsPeak")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakExceedsShoulder")) throwError = true;
    if (UtilityRoutines::SameString(objName, "ShoulderExceedsPeak")) throwError = true;
    if (UtilityRoutines::SameString(objName, "IsWinter")) throwError = true;
    if (UtilityRoutines::SameString(objName, "IsNotWinter")) throwError = true;
    if (UtilityRoutines::SameString(objName, "IsSpring")) throwError = true;
    if (UtilityRoutines::SameString(objName, "IsNotSpring")) throwError = true;
    if (UtilityRoutines::SameString(objName, "IsSummer")) throwError = true;
    if (UtilityRoutines::SameString(objName, "IsNotSummer")) throwError = true;
    if (UtilityRoutines::SameString(objName, "IsAutumn")) throwError = true;
    if (UtilityRoutines::SameString(objName, "IsNotAutumn")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakAndShoulderEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakAndShoulderDemand")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakAndMidPeakEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakAndMidPeakDemand")) throwError = true;
    if (UtilityRoutines::SameString(objName, "ShoulderAndOffPeakEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "ShoulderAndOffPeakDemand")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakAndOffPeakEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "PeakAndOffPeakDemand")) throwError = true;
    if (UtilityRoutines::SameString(objName, "RealTimePriceCosts")) throwError = true;
    if (UtilityRoutines::SameString(objName, "AboveCustomerBaseCosts")) throwError = true;
    if (UtilityRoutines::SameString(objName, "BelowCustomerBaseCosts")) throwError = true;
    if (UtilityRoutines::SameString(objName, "AboveCustomerBaseEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "BelowCustomerBaseEnergy")) throwError = true;
    if (UtilityRoutines::SameString(objName, "EnergyCharges")) throwError = true;
    if (UtilityRoutines::SameString(objName, "DemandCharges")) throwError = true;
    if (UtilityRoutines::SameString(objName, "ServiceCharges")) throwError = true;
    if (UtilityRoutines::SameString(objName, "Basis")) throwError = true;
    if (UtilityRoutines::SameString(objName, "Surcharges")) throwError = true;
    if (UtilityRoutines::SameString(objName, "Adjustments")) throwError = true;
    if (UtilityRoutines::SameString(objName, "Subtotal")) throwError = true;
    if (UtilityRoutines::SameString(objName, "Taxes")) throwError = true;
    if (UtilityRoutines::SameString(objName, "Total")) throwError = true;
    if (throwError) {
        ErrorsFound = true;
        if (curTariffIndex >= 1 && curTariffIndex <= state.dataEconTariff->numTariff) {
            ShowSevereError(state, "UtilityCost:Tariff=\"" + state.dataEconTariff->tariff(curTariffIndex).tariffName + "\" invalid referenced name");
            ShowContinueError(state, curobjName + "=\"" + objName + "\" You cannot name an object using the same name as a native variable.");
        } else {
            ShowSevereError(state, curobjName + "=\"" + objName + "\" You cannot name an object using the same name as a native variable.");
        }
    }
}

int AssignVariablePt(EnergyPlusData &state,
                     std::string const &stringIn,
                     bool const flagIfNotNumeric,
                     int const useOfVar,
                     int const varSpecific,
                     iEconVarObjType const econObjKind,
                     int const objIndex,
                     int const tariffPt)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //   If the string is not numeric, check if it is a valid string to use as
    //   a variable name. Check if name has been used before and if not create
    //   the variable using the string as its name.
    //   Return the index of the variable.

    int AssignVariablePt;

    std::string inNoSpaces;
    int found;
    int iVar;

    auto &econVar(state.dataEconTariff->econVar);

    if (flagIfNotNumeric && (len(stringIn) >= 1)) {
        inNoSpaces = RemoveSpaces(state, stringIn);
        found = 0;
        if (allocated(econVar)) {
            for (iVar = 1; iVar <= state.dataEconTariff->numEconVar; ++iVar) {
                if (econVar(iVar).tariffIndx == tariffPt) {
                    if (UtilityRoutines::SameString(econVar(iVar).name, inNoSpaces)) {
                        found = iVar;
                        break;
                    }
                }
            }
        }
        if (found > 0) {
            AssignVariablePt = found;
            if (econVar(found).kindOfObj == iEconVarObjType::Unknown) {
                econVar(found).kindOfObj = econObjKind;
                if (econVar(found).index == 0) econVar(found).index = objIndex;
            }
        } else {
            incrementEconVar(state);
            econVar(state.dataEconTariff->numEconVar).name = inNoSpaces;
            econVar(state.dataEconTariff->numEconVar).kindOfObj = econObjKind;
            econVar(state.dataEconTariff->numEconVar).index = objIndex;
            AssignVariablePt = state.dataEconTariff->numEconVar;
        }
        // now set the flag for the type of usage the variable has
        if (useOfVar == varIsArgument) {
            econVar(AssignVariablePt).isArgument = true;
        } else if (useOfVar == varIsAssigned) {
            econVar(AssignVariablePt).isAssigned = true;
        }
        econVar(AssignVariablePt).tariffIndx = tariffPt;
        // if the user defines the UtilityCost:Computation then this is called when reading the
        // UtilityCost:Tariff with varNotYetDefined but they are already defined because
        // the subroutine CreateCategoryNativeVariables has already been called.
        if (!((varSpecific == varNotYetDefined) && (econVar(AssignVariablePt).specific >= catEnergyCharges))) {
            econVar(AssignVariablePt).specific = varSpecific;
        }
    } else { // if the string was numeric return a zero
        AssignVariablePt = 0;
    }
    return AssignVariablePt;
}

void incrementEconVar(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //   Increment the Increase the size of the

    int constexpr sizeIncrement(100);

    auto &econVar(state.dataEconTariff->econVar);

    if (!allocated(econVar)) {
        econVar.allocate(sizeIncrement);
        state.dataEconTariff->sizeEconVar = sizeIncrement;
        state.dataEconTariff->numEconVar = 1;
    } else {
        ++state.dataEconTariff->numEconVar;
        // if larger than current size grow the array
        if (state.dataEconTariff->numEconVar > state.dataEconTariff->sizeEconVar) {
            econVar.redimension(state.dataEconTariff->sizeEconVar += sizeIncrement);
        }
    }
    // initialize new record) //Autodesk Most of these match default initialization so not needed
    econVar(state.dataEconTariff->numEconVar).name = "";
    econVar(state.dataEconTariff->numEconVar).tariffIndx = 0;
    econVar(state.dataEconTariff->numEconVar).kindOfObj = iEconVarObjType::Unknown;
    econVar(state.dataEconTariff->numEconVar).index = 0;
    econVar(state.dataEconTariff->numEconVar).values = 0.0;
    econVar(state.dataEconTariff->numEconVar).isArgument = false;
    econVar(state.dataEconTariff->numEconVar).isAssigned = false;
    econVar(state.dataEconTariff->numEconVar).specific = varNotYetDefined;
    //        econVar( numEconVar ).values = 0.0; //Autodesk Already initialized above
    // Autodesk Don't initialize cntMeDependOn
    econVar(state.dataEconTariff->numEconVar).Operator = 0;
    econVar(state.dataEconTariff->numEconVar).firstOperand = 1; // Autodesk Default initialization sets this to 0
    econVar(state.dataEconTariff->numEconVar).lastOperand = 0;
    econVar(state.dataEconTariff->numEconVar).activeNow = false;
    econVar(state.dataEconTariff->numEconVar).isEvaluated = false;
    // Autodesk Don't initialize isReported
    // Autodesk Don't initialize varUnitType
}

void incrementSteps(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   June 2004

    //   Increment the step array counter and if
    //   necessary increase the size of the array.

    int const sizeIncrement(100);

    if (!allocated(state.dataEconTariff->steps)) {
        state.dataEconTariff->steps.allocate(sizeIncrement);
        state.dataEconTariff->sizeSteps = sizeIncrement;
        state.dataEconTariff->numSteps = 1;
    } else {
        ++state.dataEconTariff->numSteps;
        // if larger than current size grow the array
        if (state.dataEconTariff->numSteps > state.dataEconTariff->sizeSteps) {
            state.dataEconTariff->steps.redimension(state.dataEconTariff->sizeSteps += sizeIncrement);
        }
    }
    // initialize new record
    state.dataEconTariff->steps(state.dataEconTariff->numSteps) = 0;
}

std::string RemoveSpaces(EnergyPlusData &state, std::string const &StringIn)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //   Return the string with all spaces removed.

    std::string StringOut;
    bool foundSpaces = false;
    for (std::string::size_type iString = 0; iString < len(StringIn); ++iString) {
        if (StringIn[iString] != ' ') {
            StringOut += StringIn[iString];
        } else {
            foundSpaces = true;
        }
    }
    if (foundSpaces) {
        ShowWarningError(state, "UtilityCost: Spaces were removed from the variable=\"" + StringIn + "\".");
        ShowContinueError(state, "...Resultant variable=\"" + StringOut + "\".");
    }
    return StringOut;
}

void CreateCategoryNativeVariables(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    //    For each tariff create variables that are used for the
    //    categories (i.e., EnergyCharges).

    int iTariff;

    auto &tariff(state.dataEconTariff->tariff);

    for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
        // category variables first
        tariff(iTariff).ptEnergyCharges =
            AssignVariablePt(state, "EnergyCharges", true, varIsAssigned, catEnergyCharges, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).firstCategory = state.dataEconTariff->numEconVar;
        tariff(iTariff).ptDemandCharges =
            AssignVariablePt(state, "DemandCharges", true, varIsAssigned, catDemandCharges, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).ptServiceCharges =
            AssignVariablePt(state, "ServiceCharges", true, varIsAssigned, catServiceCharges, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).ptBasis = AssignVariablePt(state, "Basis", true, varIsAssigned, catBasis, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).ptAdjustment =
            AssignVariablePt(state, "Adjustment", true, varIsAssigned, catAdjustment, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).ptSurcharge = AssignVariablePt(state, "Surcharge", true, varIsAssigned, catSurcharge, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).ptSubtotal = AssignVariablePt(state, "Subtotal", true, varIsAssigned, catSubtotal, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).ptTaxes = AssignVariablePt(state, "Taxes", true, varIsAssigned, catTaxes, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).ptTotal = AssignVariablePt(state, "Total", true, varIsAssigned, catTotal, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).ptNotIncluded =
            AssignVariablePt(state, "NotIncluded", true, varIsAssigned, catNotIncluded, iEconVarObjType::Category, 0, iTariff);
        tariff(iTariff).lastCategory = state.dataEconTariff->numEconVar;
        // category variables first
        tariff(iTariff).nativeTotalEnergy =
            AssignVariablePt(state, "TotalEnergy", true, varIsArgument, nativeTotalEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).firstNative = state.dataEconTariff->numEconVar;
        tariff(iTariff).nativeTotalDemand =
            AssignVariablePt(state, "TotalDemand", true, varIsArgument, nativeTotalDemand, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakEnergy =
            AssignVariablePt(state, "PeakEnergy", true, varIsArgument, nativePeakEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakDemand =
            AssignVariablePt(state, "PeakDemand", true, varIsArgument, nativePeakDemand, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeShoulderEnergy =
            AssignVariablePt(state, "ShoulderEnergy", true, varIsArgument, nativeShoulderEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeShoulderDemand =
            AssignVariablePt(state, "ShoulderDemand", true, varIsArgument, nativeShoulderDemand, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeOffPeakEnergy =
            AssignVariablePt(state, "OffPeakEnergy", true, varIsArgument, nativeOffPeakEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeOffPeakDemand =
            AssignVariablePt(state, "OffPeakDemand", true, varIsArgument, nativeOffPeakDemand, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeMidPeakEnergy =
            AssignVariablePt(state, "MidPeakEnergy", true, varIsArgument, nativeMidPeakEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeMidPeakDemand =
            AssignVariablePt(state, "MidPeakDemand", true, varIsArgument, nativeMidPeakDemand, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakExceedsOffPeak =
            AssignVariablePt(state, "PeakExceedsOffPeak", true, varIsArgument, nativePeakExceedsOffPeak, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeOffPeakExceedsPeak =
            AssignVariablePt(state, "OffPeakExceedsPeak", true, varIsArgument, nativeOffPeakExceedsPeak, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakExceedsMidPeak =
            AssignVariablePt(state, "PeakExceedsMidPeak", true, varIsArgument, nativePeakExceedsMidPeak, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeMidPeakExceedsPeak =
            AssignVariablePt(state, "MidPeakExceedsPeak", true, varIsArgument, nativeMidPeakExceedsPeak, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakExceedsShoulder =
            AssignVariablePt(state, "PeakExceedsShoulder", true, varIsArgument, nativePeakExceedsShoulder, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeShoulderExceedsPeak =
            AssignVariablePt(state, "ShoulderExceedsPeak", true, varIsArgument, nativeShoulderExceedsPeak, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeIsWinter =
            AssignVariablePt(state, "IsWinter", true, varIsArgument, nativeIsWinter, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeIsNotWinter =
            AssignVariablePt(state, "IsNotWinter", true, varIsArgument, nativeIsNotWinter, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeIsSpring =
            AssignVariablePt(state, "IsSpring", true, varIsArgument, nativeIsSpring, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeIsNotSpring =
            AssignVariablePt(state, "IsNotSpring", true, varIsArgument, nativeIsNotSpring, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeIsSummer =
            AssignVariablePt(state, "IsSummer", true, varIsArgument, nativeIsSummer, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeIsNotSummer =
            AssignVariablePt(state, "IsNotSummer", true, varIsArgument, nativeIsNotSummer, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeIsAutumn =
            AssignVariablePt(state, "IsAutumn", true, varIsArgument, nativeIsAutumn, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeIsNotAutumn =
            AssignVariablePt(state, "IsNotAutumn", true, varIsArgument, nativeIsNotAutumn, iEconVarObjType::Native, 0, iTariff);

        tariff(iTariff).nativePeakAndShoulderEnergy =
            AssignVariablePt(state, "PeakAndShoulderEnergy", true, varIsArgument, nativePeakAndShoulderEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakAndShoulderDemand =
            AssignVariablePt(state, "PeakAndShoulderDemand", true, varIsArgument, nativePeakAndShoulderDemand, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakAndMidPeakEnergy =
            AssignVariablePt(state, "PeakAndMidPeakEnergy", true, varIsArgument, nativePeakAndMidPeakEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakAndMidPeakDemand =
            AssignVariablePt(state, "PeakAndMidPeakDemand", true, varIsArgument, nativePeakAndMidPeakDemand, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeShoulderAndOffPeakEnergy = AssignVariablePt(
            state, "ShoulderAndOffPeakEnergy", true, varIsArgument, nativeShoulderAndOffPeakEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeShoulderAndOffPeakDemand = AssignVariablePt(
            state, "ShoulderAndOffPeakDemand", true, varIsArgument, nativeShoulderAndOffPeakDemand, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakAndOffPeakEnergy =
            AssignVariablePt(state, "PeakAndOffPeakEnergy", true, varIsArgument, nativePeakAndOffPeakEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativePeakAndOffPeakDemand =
            AssignVariablePt(state, "PeakAndOffPeakDemand", true, varIsArgument, nativePeakAndOffPeakDemand, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeRealTimePriceCosts =
            AssignVariablePt(state, "RealTimePriceCosts", true, varIsArgument, nativeRealTimePriceCosts, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeAboveCustomerBaseCosts =
            AssignVariablePt(state, "AboveCustomerBaseCosts", true, varIsArgument, nativeAboveCustomerBaseCosts, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeBelowCustomerBaseCosts =
            AssignVariablePt(state, "BelowCustomerBaseCosts", true, varIsArgument, nativeBelowCustomerBaseCosts, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeAboveCustomerBaseEnergy = AssignVariablePt(
            state, "AboveCustomerBaseEnergy", true, varIsArgument, nativeAboveCustomerBaseEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).nativeBelowCustomerBaseEnergy = AssignVariablePt(
            state, "BelowCustomerBaseEnergy", true, varIsArgument, nativeBelowCustomerBaseEnergy, iEconVarObjType::Native, 0, iTariff);
        tariff(iTariff).lastNative = state.dataEconTariff->numEconVar;
    }
}

int lookupOperator(std::string const &opString)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2004

    int lookupOperator;

    if (UtilityRoutines::SameString(opString, "Sum")) {
        lookupOperator = opSUM;
    } else if (UtilityRoutines::SameString(opString, "MULTIPLY")) {
        lookupOperator = opMULTIPLY;
    } else if (UtilityRoutines::SameString(opString, "MULT")) {
        lookupOperator = opMULTIPLY;
    } else if (UtilityRoutines::SameString(opString, "SUBTRACT")) {
        lookupOperator = opSUBTRACT;
    } else if (UtilityRoutines::SameString(opString, "SUBT")) {
        lookupOperator = opSUBTRACT;
    } else if (UtilityRoutines::SameString(opString, "DIVIDE")) {
        lookupOperator = opDIVIDE;
    } else if (UtilityRoutines::SameString(opString, "DIV")) {
        lookupOperator = opDIVIDE;
    } else if (UtilityRoutines::SameString(opString, "ABSOLUTE")) {
        lookupOperator = opABSOLUTE;
    } else if (UtilityRoutines::SameString(opString, "ABS")) {
        lookupOperator = opABSOLUTE;
    } else if (UtilityRoutines::SameString(opString, "INTEGER")) {
        lookupOperator = opINTEGER;
    } else if (UtilityRoutines::SameString(opString, "INT")) {
        lookupOperator = opINTEGER;
    } else if (UtilityRoutines::SameString(opString, "SIGN")) {
        lookupOperator = opSIGN;
    } else if (UtilityRoutines::SameString(opString, "ROUND")) {
        lookupOperator = opROUND;
    } else if (UtilityRoutines::SameString(opString, "Maximum")) {
        lookupOperator = opMAXIMUM;
    } else if (UtilityRoutines::SameString(opString, "MAX")) {
        lookupOperator = opMAXIMUM;
    } else if (UtilityRoutines::SameString(opString, "MINIMUM")) {
        lookupOperator = opMINIMUM;
    } else if (UtilityRoutines::SameString(opString, "MIN")) {
        lookupOperator = opMINIMUM;
    } else if (UtilityRoutines::SameString(opString, "EXCEEDS")) {
        lookupOperator = opEXCEEDS;
    } else if (UtilityRoutines::SameString(opString, "ANNUALMINIMUM")) {
        lookupOperator = opANNUALMINIMUM;
    } else if (UtilityRoutines::SameString(opString, "ANMIN")) {
        lookupOperator = opANNUALMINIMUM;
    } else if (UtilityRoutines::SameString(opString, "ANNUALMAXIMUM")) {
        lookupOperator = opANNUALMAXIMUM;
    } else if (UtilityRoutines::SameString(opString, "ANMAX")) {
        lookupOperator = opANNUALMAXIMUM;
    } else if (UtilityRoutines::SameString(opString, "ANNUALSUM")) {
        lookupOperator = opANNUALSUM;
    } else if (UtilityRoutines::SameString(opString, "ANSUM")) {
        lookupOperator = opANNUALSUM;
    } else if (UtilityRoutines::SameString(opString, "ANNUALAVERAGE")) {
        lookupOperator = opANNUALAVERAGE;
    } else if (UtilityRoutines::SameString(opString, "ANAVG")) {
        lookupOperator = opANNUALAVERAGE;
    } else if (UtilityRoutines::SameString(opString, "ANNUALOR")) {
        lookupOperator = opANNUALOR;
    } else if (UtilityRoutines::SameString(opString, "ANOR")) {
        lookupOperator = opANNUALOR;
    } else if (UtilityRoutines::SameString(opString, "ANNUALAND")) {
        lookupOperator = opANNUALAND;
    } else if (UtilityRoutines::SameString(opString, "ANAND")) {
        lookupOperator = opANNUALAND;
    } else if (UtilityRoutines::SameString(opString, "ANNUALMAXIMUMZERO")) {
        lookupOperator = opANNUALMAXIMUMZERO;
    } else if (UtilityRoutines::SameString(opString, "ANMAXZ")) {
        lookupOperator = opANNUALMAXIMUMZERO;
    } else if (UtilityRoutines::SameString(opString, "ANNUALMINIMUMZERO")) {
        lookupOperator = opANNUALMINIMUMZERO;
    } else if (UtilityRoutines::SameString(opString, "ANMINZ")) {
        lookupOperator = opANNUALMINIMUMZERO;
    } else if (UtilityRoutines::SameString(opString, "IF")) {
        lookupOperator = opIF;
    } else if (UtilityRoutines::SameString(opString, "GREATERTHAN")) {
        lookupOperator = opGREATERTHAN;
    } else if (UtilityRoutines::SameString(opString, "GT")) {
        lookupOperator = opGREATERTHAN;
    } else if (UtilityRoutines::SameString(opString, "GREATEREQUAL")) {
        lookupOperator = opGREATEREQUAL;
    } else if (UtilityRoutines::SameString(opString, "GE")) {
        lookupOperator = opGREATEREQUAL;
    } else if (UtilityRoutines::SameString(opString, "LESSTHAN")) {
        lookupOperator = opLESSTHAN;
    } else if (UtilityRoutines::SameString(opString, "LT")) {
        lookupOperator = opLESSTHAN;
    } else if (UtilityRoutines::SameString(opString, "LESSEQUAL")) {
        lookupOperator = opLESSEQUAL;
    } else if (UtilityRoutines::SameString(opString, "LE")) {
        lookupOperator = opLESSEQUAL;
    } else if (UtilityRoutines::SameString(opString, "EQUAL")) {
        lookupOperator = opEQUAL;
    } else if (UtilityRoutines::SameString(opString, "EQ")) {
        lookupOperator = opEQUAL;
    } else if (UtilityRoutines::SameString(opString, "NOTEQUAL")) {
        lookupOperator = opNOTEQUAL;
    } else if (UtilityRoutines::SameString(opString, "NE")) {
        lookupOperator = opNOTEQUAL;
    } else if (UtilityRoutines::SameString(opString, "AND")) {
        lookupOperator = opAND;
    } else if (UtilityRoutines::SameString(opString, "OR")) {
        lookupOperator = opOR;
    } else if (UtilityRoutines::SameString(opString, "NOT")) {
        lookupOperator = opNOT;
    } else if (UtilityRoutines::SameString(opString, "FROM")) {
        lookupOperator = opNOOP;
    } else if (UtilityRoutines::SameString(opString, "ADD")) {
        lookupOperator = opADD;
    } else {
        lookupOperator = 0;
    }
    return lookupOperator;
}

//======================================================================================================================
//======================================================================================================================

//    DEFAULT COMPUTATION RELATED ROUTINES

//======================================================================================================================
//======================================================================================================================

void CreateDefaultComputation(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   June 2004

    // PURPOSE OF THIS SUBROUTINE:
    //    For most tariffs defined in EnergyPlus no specific
    //    ECONOMICS:COMPUTATION will be entered. In that case,
    //    a default sequence of computation steps needs to be
    //    created.  This routine creates the default
    //    computation steps.
    //    Object           Fields         Depend On Fields
    //    Qualify          namePt         sourcePt
    //                                    thresholdPt
    //    Charge:Simple    namePt         sourcePt
    //                     categoryPt     costPerPt
    //    Charge:Block     namePt         sourcePt
    //                     categoryPt     blkSzMultPt
    //                     remainingPt    blkSzPt
    //                                    blkCostPt
    //    Ratchet          namePt         baselinePt
    //                                    adjustmentPt
    //                                    multiplierPt
    //                                    offsetPt
    //    These will be formed into expressions that look like
    //      namePt NOOP sourcePt thresholdPt
    //    The different Charges are combined using the SUM operation
    //    into categories.
    //      category SUM chg1Name chg2Name chg3Name
    //    Since the dependency array has one target and multiple
    //    parameters, remainingPt is shown as a seperate equation that
    //    depends on namePt for Charge:Block. The equation will not be
    //    displayed or processed except in the sort.
    //      remainingPt NOOP namePt
    //    Many lines of the computation will include just the name of
    //    a single variable which triggers the calculation for that
    //    charge, ratchet or qualify.
    //      chg1Name
    //    It is also possible that two variables referenced within one
    //    object could include a dependancy relationship also. For
    //    example, the blkSzPt could be calculated using the same sourePt
    //    in Charge:Block.

    // METHODOLOGY EMPLOYED:
    //    Since some ECONOMCIS:* objects depend on other variables
    //    first must create the order of when to perform the
    //    computations. First a dependancy table is created that
    //    indicates what variables are dependant on other variables.
    //    A directed acyclic graph (DAG) describes the general
    //    problem which is usually solved using a topological
    //    sorting algorithm.
    //    Each line/step is generated and put into the depend
    //    array. Also in the array are counts of how many items it
    //    depends on and a list of entries that are dependant on that
    //    line.

    int iTariff;
    int iVar;
    int jVar;
    int kObj;
    int mBlock;
    int kOperand;
    int curBasis;
    int curSubtotal;
    int curTotal;
    int curObject;
    int numNoDepend;
    int referVar;
    int loopCount;
    bool remainingVarFlag;
    int remainPt;

    auto &tariff(state.dataEconTariff->tariff);
    auto &econVar(state.dataEconTariff->econVar);
    auto &computation(state.dataEconTariff->computation);
    auto &qualify(state.dataEconTariff->qualify);
    auto &ratchet(state.dataEconTariff->ratchet);
    auto &chargeSimple(state.dataEconTariff->chargeSimple);
    auto &chargeBlock(state.dataEconTariff->chargeBlock);

    // for each tariff that does not have a UtilityCost:Computation object go through the variables
    for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
        if (!computation(iTariff).isUserDef) {
            // clear all variables so that they are not active
            for (jVar = 1; jVar <= state.dataEconTariff->numEconVar; ++jVar) {
                econVar(jVar).activeNow = false;
            }
            // make all native variables active
            for (jVar = tariff(iTariff).firstNative; jVar <= tariff(iTariff).lastNative; ++jVar) {
                econVar(jVar).activeNow = true;
            }
            //"clear" the dependOn array
            state.dataEconTariff->numOperand = 0;
            // Define the preset equations (category sumation)
            curTotal = tariff(iTariff).ptTotal;
            curSubtotal = tariff(iTariff).ptSubtotal;
            curBasis = tariff(iTariff).ptBasis;
            // total SUM subtotal taxes
            econVar(curTotal).Operator = opSUM;
            econVar(curTotal).activeNow = true;
            addOperand(state, curTotal, curSubtotal);
            addOperand(state, curTotal, tariff(iTariff).ptTaxes);
            // subtotal SUM basis adjustments surcharges
            econVar(curSubtotal).Operator = opSUM;
            econVar(curSubtotal).activeNow = true;
            addOperand(state, curSubtotal, curBasis);
            addOperand(state, curSubtotal, tariff(iTariff).ptAdjustment);
            addOperand(state, curSubtotal, tariff(iTariff).ptSurcharge);
            // basis SUM EnergyCharges DemandCharges ServiceCharges
            econVar(curBasis).Operator = opSUM;
            econVar(curBasis).activeNow = true;
            addOperand(state, curBasis, tariff(iTariff).ptEnergyCharges);
            addOperand(state, curBasis, tariff(iTariff).ptDemandCharges);
            addOperand(state, curBasis, tariff(iTariff).ptServiceCharges);
            // set up the equations for other objects
            addChargesToOperand(state, iTariff, tariff(iTariff).ptEnergyCharges);
            addChargesToOperand(state, iTariff, tariff(iTariff).ptDemandCharges);
            addChargesToOperand(state, iTariff, tariff(iTariff).ptServiceCharges);
            addChargesToOperand(state, iTariff, tariff(iTariff).ptAdjustment);
            addChargesToOperand(state, iTariff, tariff(iTariff).ptSurcharge);
            addChargesToOperand(state, iTariff, tariff(iTariff).ptTaxes);
            // add the real time pricing to the energy charges
            if (tariff(iTariff).chargeSchIndex != 0) {
                addOperand(state, tariff(iTariff).ptEnergyCharges, tariff(iTariff).nativeRealTimePriceCosts);
            }
            // now add equations with NOOP to represent each object with its
            // dependancies
            // Qualify
            for (kObj = 1; kObj <= state.dataEconTariff->numQualify; ++kObj) {
                if (qualify(kObj).tariffIndx == iTariff) {
                    curObject = qualify(kObj).namePt;
                    econVar(curObject).Operator = opNOOP;
                    econVar(curObject).activeNow = true;
                    addOperand(state, curObject, qualify(kObj).sourcePt);
                    addOperand(state, curObject, qualify(kObj).thresholdPt);
                }
            }
            // Ratchet
            for (kObj = 1; kObj <= state.dataEconTariff->numRatchet; ++kObj) {
                if (ratchet(kObj).tariffIndx == iTariff) {
                    curObject = ratchet(kObj).namePt;
                    econVar(curObject).Operator = opNOOP;
                    econVar(curObject).activeNow = true;
                    addOperand(state, curObject, ratchet(kObj).baselinePt);
                    addOperand(state, curObject, ratchet(kObj).adjustmentPt);
                    addOperand(state, curObject, ratchet(kObj).multiplierPt);
                    addOperand(state, curObject, ratchet(kObj).offsetPt);
                }
            }
            // ChargeSimple
            for (kObj = 1; kObj <= state.dataEconTariff->numChargeSimple; ++kObj) {
                if (chargeSimple(kObj).tariffIndx == iTariff) {
                    curObject = chargeSimple(kObj).namePt;
                    econVar(curObject).Operator = opNOOP;
                    econVar(curObject).activeNow = true;
                    addOperand(state, curObject, chargeSimple(kObj).sourcePt);
                    addOperand(state, curObject, chargeSimple(kObj).costPerPt);
                }
            }
            // ChargeBlock
            for (kObj = 1; kObj <= state.dataEconTariff->numChargeBlock; ++kObj) {
                if (chargeBlock(kObj).tariffIndx == iTariff) {
                    curObject = chargeBlock(kObj).namePt;
                    econVar(curObject).Operator = opNOOP;
                    econVar(curObject).activeNow = true;
                    addOperand(state, curObject, chargeBlock(kObj).sourcePt);
                    addOperand(state, curObject, chargeBlock(kObj).blkSzMultPt);
                    for (mBlock = 1; mBlock <= chargeBlock(kObj).numBlk; ++mBlock) {
                        addOperand(state, curObject, chargeBlock(kObj).blkSzPt(mBlock));
                        addOperand(state, curObject, chargeBlock(kObj).blkCostPt(mBlock));
                    }
                    // now add a new "equation" for dependency of remainingPt on namePt
                    remainPt = chargeBlock(kObj).remainingPt;
                    if (remainPt > 0) {
                        econVar(remainPt).Operator = opNOOP;
                        econVar(remainPt).activeNow = true;
                        addOperand(state, remainPt, curObject);
                    }
                }
            }
            // Economic:Variable
            // make all of the user defined variables as active
            for (iVar = 1; iVar <= state.dataEconTariff->numEconVar; ++iVar) {
                if (econVar(iVar).tariffIndx == iTariff) {
                    if (econVar(iVar).kindOfObj == iEconVarObjType::Variable) {
                        econVar(iVar).activeNow = true;
                    }
                }
            }
            // make sure no compuation is already user defined
            if (computation(iTariff).firstStep != 0) {
                ShowWarningError(state, "In UtilityCost:Tariff: Overwriting user defined tariff " + tariff(iTariff).tariffName);
            }
            // initialize the computation
            computation(iTariff).computeName = "Autogenerated - " + tariff(iTariff).tariffName;
            computation(iTariff).firstStep = state.dataEconTariff->numSteps + 1;
            computation(iTariff).lastStep = -1; // this will be incremented by addStep
            computation(iTariff).isUserDef = false;
            // now all "equations" are defined, treat the variables with the list
            // of dependancies as a directed acyclic graph and use "count down" algorithm
            // to do a topological sort of the variables into the order for computation
            // First, clear the counters
            for (jVar = 1; jVar <= state.dataEconTariff->numEconVar; ++jVar) {
                econVar(jVar).cntMeDependOn = 0;
            }
            // Second, add up the number of dependancies on each variable
            for (iVar = 1; iVar <= state.dataEconTariff->numEconVar; ++iVar) {
                if (econVar(iVar).activeNow) {
                    if (econVar(iVar).lastOperand >= econVar(iVar).firstOperand) {
                        econVar(iVar).cntMeDependOn = 1 + econVar(iVar).lastOperand - econVar(iVar).firstOperand;
                    }
                }
            }
            // Third, start removing items with zero connections and decrease each
            //   counter.
            numNoDepend = -1;
            loopCount = 0;
            while ((numNoDepend != 0) || (loopCount > 100000)) {
                numNoDepend = 0;
                for (iVar = 1; iVar <= state.dataEconTariff->numEconVar; ++iVar) {
                    if (econVar(iVar).activeNow) {
                        // find a variable that has no more dangling dependancies
                        if (econVar(iVar).cntMeDependOn == 0) {
                            // If the variable is a native variable then
                            // IF (econVar(iVar)%kindOfObj .NE. iEconVarObjType::Native) THEN
                            if ((econVar(iVar).kindOfObj != iEconVarObjType::Native) && (econVar(iVar).kindOfObj != iEconVarObjType::Variable)) {
                                if (econVar(iVar).lastOperand >= econVar(iVar).firstOperand) {
                                    // transfer variables and operator to the computation and list of steps
                                    // go through the operands backwards (end of line is evaluated first)
                                    for (kOperand = econVar(iVar).lastOperand; kOperand >= econVar(iVar).firstOperand; --kOperand) {
                                        incrementSteps(state);
                                        state.dataEconTariff->steps(state.dataEconTariff->numSteps) = state.dataEconTariff->operand(kOperand);
                                    }
                                    // append the operator (either SUM or NOOP)
                                    incrementSteps(state);
                                    state.dataEconTariff->steps(state.dataEconTariff->numSteps) = econVar(iVar).Operator;
                                    // append the variable itself
                                    incrementSteps(state);
                                    state.dataEconTariff->steps(state.dataEconTariff->numSteps) = iVar;
                                    // at the end of the line show a zero to clear the stack
                                    incrementSteps(state);
                                    state.dataEconTariff->steps(state.dataEconTariff->numSteps) = 0;
                                }
                            }
                            // go through each other variable looking for places where this variable is used
                            // and decrement their counters.
                            for (jVar = 1; jVar <= state.dataEconTariff->numEconVar; ++jVar) {
                                if (econVar(jVar).activeNow) {
                                    for (kOperand = econVar(jVar).firstOperand; kOperand <= econVar(jVar).lastOperand; ++kOperand) {
                                        referVar = state.dataEconTariff->operand(kOperand);
                                        if (iVar == referVar) {
                                            --econVar(jVar).cntMeDependOn;
                                            // for each variable that has been decremented to zero increment the counter
                                            if (econVar(jVar).cntMeDependOn <= 0) {
                                                ++numNoDepend;
                                            }
                                        }
                                    }
                                }
                            }
                            // make the variable inactive
                            econVar(iVar).activeNow = false;
                        }
                    }
                }
                ++loopCount;
            }
            if (loopCount > 100000) {
                ShowWarningError(state,
                                 "UtilityCost:Tariff: Loop count exceeded when counting dependancies in tariff: " + tariff(iTariff).tariffName);
            }
            // make sure that all variables associated with the tariff are included
            remainingVarFlag = false;
            for (iVar = 1; iVar <= state.dataEconTariff->numEconVar; ++iVar) {
                if (econVar(iVar).activeNow) {
                    remainingVarFlag = true;
                }
            }
            if (remainingVarFlag) {
                ShowWarningError(state,
                                 "CreateDefaultComputation: In UtilityCost:Computation: Circular or invalid dependencies found in tariff: " +
                                     tariff(iTariff).tariffName);
                ShowContinueError(state, "  UtilityCost variables that may have invalid dependencies and the variables they are dependant on.");
                for (iVar = 1; iVar <= state.dataEconTariff->numEconVar; ++iVar) {
                    if (econVar(iVar).tariffIndx == iTariff) {
                        if (econVar(iVar).activeNow) {
                            ShowContinueError(state, "     " + econVar(iVar).name);
                            for (kOperand = econVar(iVar).firstOperand; kOperand <= econVar(iVar).lastOperand; ++kOperand) {
                                ShowContinueError(state, "        ->  " + econVar(state.dataEconTariff->operand(kOperand)).name);
                            }
                        }
                    }
                }
            }
            // set the end of the computations
            computation(iTariff).lastStep = state.dataEconTariff->numSteps;
            if (computation(iTariff).firstStep >= computation(iTariff).lastStep) {
                computation(iTariff).firstStep = 0;
                computation(iTariff).lastStep = -1;
                ShowWarningError(state,
                                 "CreateDefaultComputation: In UtilityCost:Computation: No lines in the auto generated computation can be "
                                 "interpreted in tariff: " +
                                     tariff(iTariff).tariffName);
            }
        }
    }
}

void addOperand(EnergyPlusData &state, int const varMe, int const varOperand)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //   Used by CreateDefaultComputation to create the dependancy
    //   relationship in the EconVar array

    int constexpr sizeIncrement(100);

    auto &econVar(state.dataEconTariff->econVar);

    if (varOperand != 0) {
        // increment the numOperand and allocate/reallocate the array
        // if necessary
        if (!allocated(state.dataEconTariff->operand)) {
            state.dataEconTariff->operand.allocate(sizeIncrement);
            state.dataEconTariff->sizeOperand = sizeIncrement;
            state.dataEconTariff->numOperand = 1;
        } else {
            ++state.dataEconTariff->numOperand;
            // if larger than current size grow the array
            if (state.dataEconTariff->numOperand > state.dataEconTariff->sizeOperand) {
                state.dataEconTariff->operand.redimension(state.dataEconTariff->sizeOperand += sizeIncrement);
            }
        }
        // now add the dependency relationship
        state.dataEconTariff->operand(state.dataEconTariff->numOperand) = varOperand;
        econVar(varMe).lastOperand = state.dataEconTariff->numOperand;
        // if it is the first time addOperand was called with the varMe value
        // then set the first pointer as well
        if (varMe != state.dataEconTariff->addOperand_prevVarMe) {
            econVar(varMe).firstOperand = state.dataEconTariff->numOperand;
            state.dataEconTariff->addOperand_prevVarMe = varMe;
        }
    }
}

void addChargesToOperand(EnergyPlusData &state, int const curTariff, int const curPointer)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //   Used by CreateDefaultComputation to create the "equation"
    //   for the categories that are summations of ECONOMICS:CHARGES:BLOCK
    //   and ECONOMICS:CHARGES:SIMPLE

    int kObj;

    auto &econVar(state.dataEconTariff->econVar);
    auto &chargeSimple(state.dataEconTariff->chargeSimple);
    auto &chargeBlock(state.dataEconTariff->chargeBlock);

    econVar(curPointer).Operator = opSUM;
    econVar(curPointer).activeNow = true;
    for (kObj = 1; kObj <= state.dataEconTariff->numChargeSimple; ++kObj) {
        if (chargeSimple(kObj).tariffIndx == curTariff) {
            if (chargeSimple(kObj).categoryPt == curPointer) {
                addOperand(state, curPointer, chargeSimple(kObj).namePt);
            }
        }
    }
    for (kObj = 1; kObj <= state.dataEconTariff->numChargeBlock; ++kObj) {
        if (chargeBlock(kObj).tariffIndx == curTariff) {
            if (chargeBlock(kObj).categoryPt == curPointer) {
                addOperand(state, curPointer, chargeBlock(kObj).namePt);
            }
        }
    }
}

//======================================================================================================================
//======================================================================================================================

//    GATHER TIMESTEP VALUES ROUTINE

//======================================================================================================================
//======================================================================================================================

void GatherForEconomics(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   June 2004

    //   Gathers the data each timestep and updates the arrays
    //   holding the data that will be used by the tariff
    //   calculation.

    using ScheduleManager::GetCurrentScheduleValue;

    int iTariff;
    Real64 curInstantValue;
    Real64 curDemand;
    Real64 curEnergy;
    bool isGood;
    int curSeason;
    int curMonth;
    int curPeriod;
    Real64 curRTPprice;    // real time price
    Real64 curRTPbaseline; // real time price customer baseline load
    Real64 curRTPenergy;   // energy applied to real time price
    Real64 curRTPcost;     // cost for energy for current time

    auto &tariff(state.dataEconTariff->tariff);

    if (state.dataEconTariff->numTariff >= 1) {
        for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
            isGood = false;
            // if the meter is defined get the value
            if (tariff(iTariff).reportMeterIndx != 0) {
                curInstantValue = GetCurrentMeterValue(state, tariff(iTariff).reportMeterIndx);
            } else {
                curInstantValue = 0.0;
            }
            // remember the demand is still energy over a period of time divided by the
            // length of time. This gathers the energy also.
            tariff(iTariff).collectEnergy += curInstantValue;
            tariff(iTariff).collectTime += state.dataGlobal->TimeStepZoneSec;
            // added *SecInHour when adding RTP support August 2008
            if (tariff(iTariff).collectTime >= tariff(iTariff).demWinTime * DataGlobalConstants::SecInHour) {
                // get current value that has been converted into desired units
                curDemand = tariff(iTariff).demandConv * tariff(iTariff).collectEnergy / tariff(iTariff).collectTime;
                curEnergy = tariff(iTariff).energyConv * tariff(iTariff).collectEnergy;
                // get the schedule values
                // remember no confirmation of schedule values occurs prior to now
                if (tariff(iTariff).seasonSchIndex != 0) {
                    curSeason = GetCurrentScheduleValue(state, tariff(iTariff).seasonSchIndex);
                } else {
                    curSeason = 1;
                }
                if (tariff(iTariff).periodSchIndex != 0) {
                    curPeriod = GetCurrentScheduleValue(state, tariff(iTariff).periodSchIndex);
                } else {
                    curPeriod = 1;
                }
                if (tariff(iTariff).monthSchIndex != 0) {
                    curMonth = GetCurrentScheduleValue(state, tariff(iTariff).monthSchIndex);
                } else {
                    // #7814 - Have to be careful with DST. tariff::seasonForMonth is overwritten at each timestep, and only the last value is
                    // retained, so make sure to capture the right one
                    if ((state.dataGlobal->HourOfDay + state.dataEnvrn->DSTIndicator) <= 24) {
                        curMonth = state.dataEnvrn->Month;
                    } else {
                        curMonth = state.dataEnvrn->MonthTomorrow;
                    }
                }
                if (isWithinRange(state, curSeason, 1, 5)) {
                    if (isWithinRange(state, curPeriod, 1, 4)) {
                        if (isWithinRange(state, curMonth, 1, 12)) {
                            isGood = true;
                        }
                    }
                }
                if (isGood) {
                    tariff(iTariff).seasonForMonth(curMonth) = curSeason;
                    tariff(iTariff).gatherEnergy(curMonth, curPeriod) += curEnergy;
                    if (tariff(iTariff).gatherDemand(curMonth, curPeriod) < curDemand) {
                        tariff(iTariff).gatherDemand(curMonth, curPeriod) = curDemand;
                    }
                } else {
                    ShowWarningError(state, "UtilityCost:Tariff: While gathering for: " + tariff(iTariff).tariffName);
                    ShowContinueError(state, "Invalid schedule values - outside of range");
                }
                // Real Time Pricing
                if (tariff(iTariff).chargeSchIndex != 0) {
                    curRTPprice = GetCurrentScheduleValue(state, tariff(iTariff).chargeSchIndex);
                    // if customer baseline load schedule is used, subtract that off of the
                    // current energy
                    if (tariff(iTariff).baseUseSchIndex != 0) {
                        curRTPbaseline = GetCurrentScheduleValue(state, tariff(iTariff).baseUseSchIndex);
                        curRTPenergy = curEnergy - curRTPbaseline;
                    } else {
                        curRTPenergy = curEnergy;
                    }
                    // calculate the real time cost for current times energy
                    curRTPcost = curRTPenergy * curRTPprice;
                    tariff(iTariff).RTPcost(curMonth) += curRTPcost;
                    if (curRTPcost > 0) {
                        tariff(iTariff).RTPaboveBaseCost(curMonth) += curRTPcost;
                    } else {
                        tariff(iTariff).RTPbelowBaseCost(curMonth) += curRTPcost;
                    }
                    if (curRTPenergy > 0) {
                        tariff(iTariff).RTPaboveBaseEnergy(curMonth) += curRTPenergy;
                    } else {
                        tariff(iTariff).RTPbelowBaseEnergy(curMonth) += curRTPenergy;
                    }
                }
                // reset the counters
                tariff(iTariff).collectEnergy = 0.0;
                tariff(iTariff).collectTime = 0.0;
            }
        }
    }
}

bool isWithinRange(EnergyPlusData &state, int const testVal, int const minThreshold, int const maxThreshold)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //   Simple function to check if an integer is equal to or between
    //   two other values.

    bool isWithinRange;

    if (maxThreshold < minThreshold) {
        ShowWarningError(state, "UtilityCost: Invalid thresholds in IsWithinRange routine.");
    }
    if ((testVal <= maxThreshold) && (testVal >= minThreshold)) {
        isWithinRange = true;
    } else {
        isWithinRange = false;
    }
    return isWithinRange;
}

//======================================================================================================================
//======================================================================================================================

//    COMPUTE THE UTILITY BILLS AND CREATE REPORTS

//======================================================================================================================
//======================================================================================================================

void ComputeTariff(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //    Perform the calculation steps to compute the monthly
    //    utility bills for the user entered tariffs.
    //    The list of steps for the tariff computation are in order
    //    for stack based computation (reverse polish notation)

    // values used in specific operations
    Array1D<Real64> a(MaxNumMonths);
    int aPt;
    Array1D<Real64> b(MaxNumMonths);
    int bPt;
    Array1D<Real64> c(MaxNumMonths);
    int cPt;
    Array1D<Real64> d(MaxNumMonths);

    int iTariff;
    int jStep;
    int lMonth;
    int nVar;
    int curStep;
    int const noVar(0);

    Real64 hugeValue;
    Real64 annualAggregate;
    int annualCnt;

    auto &econVar(state.dataEconTariff->econVar);
    auto &computation(state.dataEconTariff->computation);

    if (!(state.files.outputControl.tabular || state.files.outputControl.sqlite)) {
        state.dataOutRptTab->WriteTabularFiles = false;
        return;
    }

    hugeValue = HUGE_(Real64());
    //  Clear the isEvaluated flags for all economics variables.
    for (nVar = 1; nVar <= state.dataEconTariff->numEconVar; ++nVar) {
        econVar(nVar).isEvaluated = false;
    }
    if (state.dataEconTariff->numTariff >= 1) {
        state.dataOutRptTab->WriteTabularFiles = true;
        setNativeVariables(state);
        for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
            for (jStep = computation(iTariff).firstStep; jStep <= computation(iTariff).lastStep; ++jStep) {
                curStep = state.dataEconTariff->steps(jStep);
                {
                    auto const SELECT_CASE_var(curStep);
                    if (SELECT_CASE_var == 0) { // end of line - assign variable and clear stack
                        // if the stack still has two items on it then assign the values to the
                        // pointer otherwise if it follows a NOOP line it will only have one item
                        // that has already been assigned and no further action is required.
                        if (state.dataEconTariff->topOfStack >= 2) {
                            popStack(state, b, bPt); // pop the variable pointer
                            popStack(state, a, aPt); // pop the values
                            if (isWithinRange(state, bPt, 1, state.dataEconTariff->numEconVar)) {
                                econVar(bPt).values = a;
                            }
                        }
                        state.dataEconTariff->topOfStack = 0;
                    } else if ((SELECT_CASE_var >= 1)) { // all positive values are a reference to an econVar
                        pushStack(state, econVar(curStep).values, curStep);
                    } else if (SELECT_CASE_var == opSUM) {
                        a = 0.0;
                        for (int kStack = 1, kStack_end = state.dataEconTariff->topOfStack; kStack <= kStack_end;
                             ++kStack) { // popStack modifies topOfStack
                            popStack(state, b, bPt);
                            a += b;
                        }
                        pushStack(state, a, noVar);
                    } else if (SELECT_CASE_var == opMULTIPLY) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        pushStack(state, a * b, noVar);
                    } else if (SELECT_CASE_var == opSUBTRACT) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        pushStack(state, b - a, noVar);
                    } else if (SELECT_CASE_var == opDIVIDE) {
                        popStack(state, a, aPt);
                        popStack(state, b, bPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (b(lMonth) != 0) {
                                c(lMonth) = a(lMonth) / b(lMonth);
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opABSOLUTE) {
                        popStack(state, a, aPt);
                        pushStack(state, ObjexxFCL::abs(a), noVar);
                    } else if (SELECT_CASE_var == opINTEGER) {
                        popStack(state, a, aPt);
                        pushStack(state, Array1D_double(Array1D_int(a)), noVar);
                    } else if (SELECT_CASE_var == opSIGN) {
                        popStack(state, a, aPt);
                        pushStack(state, sign(1.0, a), noVar);
                        //        CASE (opROUND)
                        //          CALL popStack(b,bPt)
                        //          CALL popStack(a,aPt)
                        //          DO lMonth = 1,MaxNumMonths
                        //            IF ((b(lMonth) .LE. 5) .AND. (b(lMonth) .GE. -5)) THEN
                        //              c(lMonth) = FLOAT(INT(a(lMonth) / (10 ** b(lMonth))) * (10 ** b(lMonth)))
                        //            END IF
                        //          END DO
                        //          CALL pushStack(c,noVar)
                    } else if (SELECT_CASE_var == opMAXIMUM) {
                        a = -hugeValue;
                        for (int kStack = 1, kStack_end = state.dataEconTariff->topOfStack; kStack <= kStack_end;
                             ++kStack) { // popStack modifies topOfStack
                            popStack(state, b, bPt);
                            for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                                if (b(lMonth) > a(lMonth)) {
                                    a(lMonth) = b(lMonth);
                                }
                            }
                        }
                        pushStack(state, a, noVar);
                    } else if (SELECT_CASE_var == opMINIMUM) {
                        a = hugeValue;
                        for (int kStack = 1, kStack_end = state.dataEconTariff->topOfStack; kStack <= kStack_end;
                             ++kStack) { // popStack modifies topOfStack
                            popStack(state, b, bPt);
                            for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                                if (b(lMonth) < a(lMonth)) {
                                    a(lMonth) = b(lMonth);
                                }
                            }
                        }
                        pushStack(state, a, noVar);
                    } else if (SELECT_CASE_var == opEXCEEDS) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) > b(lMonth)) {
                                c(lMonth) = a(lMonth) - b(lMonth);
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opANNUALMINIMUM) {
                        // takes the minimum but ignores zeros
                        annualAggregate = hugeValue;
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) != 0) {
                                if (a(lMonth) < annualAggregate) {
                                    annualAggregate = a(lMonth);
                                }
                            }
                        }
                        // if all months are zero then hugeValue still in annual but should be zero
                        if (annualAggregate == hugeValue) {
                            annualAggregate = 0.0;
                        }
                        c = annualAggregate;
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opANNUALMAXIMUM) {
                        // takes the maximum but ignores zeros
                        annualAggregate = -hugeValue;
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) != 0) {
                                if (a(lMonth) > annualAggregate) {
                                    annualAggregate = a(lMonth);
                                }
                            }
                        }
                        // if all months are zero then hugeValue still in annual but should be zero
                        if (annualAggregate == -hugeValue) {
                            annualAggregate = 0.0;
                        }
                        c = annualAggregate;
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opANNUALSUM) {
                        // takes the maximum but ignores zeros
                        annualAggregate = 0.0;
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            annualAggregate += a(lMonth);
                        }
                        c = annualAggregate;
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opANNUALAVERAGE) {
                        // takes the annual sum but ignores zeros
                        annualAggregate = 0.0;
                        annualCnt = 0;
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) != 0) {
                                annualAggregate += a(lMonth);
                                ++annualCnt;
                            }
                        }
                        // if all months are zero then return zero
                        if (annualCnt != 0) {
                            c = annualAggregate / annualCnt;
                        } else {
                            c = 0.0;
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opANNUALOR) {
                        annualCnt = 0;
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) != 0) {
                                ++annualCnt;
                            }
                        }
                        // if any months is not zero then "true"
                        if (annualCnt >= 1) {
                            c = 1.0;
                        } else {
                            c = 0.0;
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opANNUALAND) {
                        annualCnt = 0;
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) != 0) {
                                ++annualCnt;
                            }
                        }
                        // if all months are not zero then "true"
                        if (annualCnt == MaxNumMonths) {
                            c = 1.0;
                        } else {
                            c = 0.0;
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opANNUALMAXIMUMZERO) {
                        // takes the maximum including zeros
                        annualAggregate = -hugeValue;
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) > annualAggregate) {
                                annualAggregate = a(lMonth);
                            }
                        }
                        c = annualAggregate;
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opANNUALMINIMUMZERO) {
                        // takes the maximum including zeros
                        annualAggregate = hugeValue;
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) < annualAggregate) {
                                annualAggregate = a(lMonth);
                            }
                        }
                        c = annualAggregate;
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opIF) {
                        popStack(state, c, cPt);
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) != 0) {
                                d(lMonth) = b(lMonth);
                            } else {
                                d(lMonth) = c(lMonth);
                            }
                        }
                        pushStack(state, d, noVar);
                    } else if (SELECT_CASE_var == opGREATERTHAN) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) > b(lMonth)) {
                                c(lMonth) = 1.0;
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opGREATEREQUAL) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) >= b(lMonth)) {
                                c(lMonth) = 1.0;
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opLESSTHAN) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) < b(lMonth)) {
                                c(lMonth) = 1.0;
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opLESSEQUAL) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) <= b(lMonth)) {
                                c(lMonth) = 1.0;
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opEQUAL) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) == b(lMonth)) {
                                c(lMonth) = 1.0;
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opNOTEQUAL) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) != b(lMonth)) {
                                c(lMonth) = 1.0;
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opAND) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if ((a(lMonth) != 0) && (b(lMonth) != 0)) {
                                c(lMonth) = 1.0;
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opOR) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if ((a(lMonth) != 0) || (b(lMonth) != 0)) {
                                c(lMonth) = 1.0;
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opNOT) {
                        popStack(state, a, aPt);
                        for (lMonth = 1; lMonth <= MaxNumMonths; ++lMonth) {
                            if (a(lMonth) == 0) {
                                c(lMonth) = 1.0;
                            } else {
                                c(lMonth) = 0.0;
                            }
                        }
                        pushStack(state, c, noVar);
                    } else if (SELECT_CASE_var == opADD) {
                        popStack(state, b, bPt);
                        popStack(state, a, aPt);
                        pushStack(state, a + b, noVar);
                    } else if (SELECT_CASE_var == opNOOP) {
                        // do nothing but clear the stack
                        state.dataEconTariff->topOfStack = 0;
                        // No longer pushing a zero to fix bug
                        // and push zero
                        // a = 0
                    }
                }
            }
            checkMinimumMonthlyCharge(state, iTariff);
        }
        selectTariff(state);
        LEEDtariffReporting(state);
    }
}

void pushStack(EnergyPlusData &state, Array1A<Real64> const monthlyArray, int const variablePointer)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //    A stack is used in the evaluation of the tariff since
    //    the variables and operators are in a reverse polish
    //    notation order. The stack operates on a last-in
    //    first out basis. The stack consists of both a pointer
    //    to the variable and the twelve monthly values.
    //    This routine puts an item on the top of the stack.

    monthlyArray.dim(MaxNumMonths);

    Array1D<Real64> curMonthlyArray(MaxNumMonths);
    int constexpr sizeIncrement(50);

    auto &stack(state.dataEconTariff->stack);
    auto &econVar(state.dataEconTariff->econVar);
    auto &tariff(state.dataEconTariff->tariff);

    curMonthlyArray = monthlyArray;
    if (!allocated(stack)) {
        stack.allocate(sizeIncrement);
        state.dataEconTariff->sizeStack = sizeIncrement;
        state.dataEconTariff->topOfStack = 1;
    } else {
        ++state.dataEconTariff->topOfStack;
        // if larger than current size grow the array
        if (state.dataEconTariff->topOfStack > state.dataEconTariff->sizeStack) {
            stack.redimension(state.dataEconTariff->sizeStack += sizeIncrement);
        }
    }
    // now push the values on to the stack
    stack(state.dataEconTariff->topOfStack).varPt = variablePointer;
    // check if variable has been evaluated if it is CHARGE:SIMPLE, CHARGE:BLOCK, RATCHET, or QUALIFY
    // if it has not overwrite the values for monthlyArray with the evaluated values
    if (variablePointer != 0) {
        if (!econVar(variablePointer).isEvaluated) {

            switch (econVar(variablePointer).kindOfObj) {
            case iEconVarObjType::ChargeSimple:
                evaluateChargeSimple(state, variablePointer);
                break;
            case iEconVarObjType::ChargeBlock:
                evaluateChargeBlock(state, variablePointer);
                break;
            case iEconVarObjType::Ratchet:
                evaluateRatchet(state, variablePointer);
                break;
            case iEconVarObjType::Qualify:
                evaluateQualify(state, variablePointer);
                break;
            case iEconVarObjType::Unknown:
                ShowWarningError(state, "UtilityCost variable not defined: " + econVar(variablePointer).name);
                ShowContinueError(state, "   In tariff: " + tariff(econVar(variablePointer).tariffIndx).tariffName);
                ShowContinueError(state, "   This may be the result of a misspelled variable name in the UtilityCost:Computation object.");
                ShowContinueError(state, "   All zero values will be assumed for this variable.");
                break;
            case iEconVarObjType::Variable:
            case iEconVarObjType::Category:
            case iEconVarObjType::Native:
            case iEconVarObjType::AssignCompute:
            case iEconVarObjType::Tariff:
            case iEconVarObjType::Computation:
                // do nothing
                break;
            default:
                ShowWarningError(state,
                                 format("UtilityCost Debugging issue. Invalid kind of variable used (pushStack). {} in tariff: {}",
                                        econVar(variablePointer).kindOfObj,
                                        tariff(econVar(variablePointer).tariffIndx).tariffName));
            }
            // if the serviceCharges are being evaluated add in the monthly charges
            if (econVar(variablePointer).specific == catServiceCharges) addMonthlyCharge(state, variablePointer);
            // get the results of performing the evaulation - should have been
            // put into the econVar values
            curMonthlyArray = econVar(variablePointer).values;
        }
    }
    // now assign
    stack(state.dataEconTariff->topOfStack).values = curMonthlyArray;
}

void popStack(EnergyPlusData &state, Array1A<Real64> monthlyArray, int &variablePointer)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //    A stack is used in the evaluation of the tariff since
    //    the variables and operators are in a reverse polish
    //    notation order. The stack operates on a last-in
    //    first out basis. The stack consists of both a pointer
    //    to the variable and the twelve monthly values.
    //    This routine returns the item on the top of the stack
    //    and removes it from the stack.

    monthlyArray.dim(MaxNumMonths);

    auto &stack(state.dataEconTariff->stack);

    if (state.dataEconTariff->topOfStack >= 1) {
        variablePointer = stack(state.dataEconTariff->topOfStack).varPt;
        monthlyArray = stack(state.dataEconTariff->topOfStack).values;
    } else {
        ShowWarningError(state,
                         "UtilityCost:Tariff: stack underflow in calculation of utility bills. On variable: " +
                             state.dataEconTariff->econVar(variablePointer).name);
        variablePointer = 0;
        monthlyArray = 0.0;
        state.dataEconTariff->topOfStack = 0;
    }
    --state.dataEconTariff->topOfStack;
}

void evaluateChargeSimple(EnergyPlusData &state, int const usingVariable)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    int curTariff;
    int indexInChg;
    Array1D<Real64> sourceVals(MaxNumMonths);
    Array1D<Real64> costPer(MaxNumMonths);
    Array1D<Real64> resultChg(MaxNumMonths);
    Array1D<Real64> seasonMask(MaxNumMonths);

    auto &econVar(state.dataEconTariff->econVar);
    auto &chargeSimple(state.dataEconTariff->chargeSimple);
    auto &tariff(state.dataEconTariff->tariff);

    curTariff = econVar(usingVariable).tariffIndx;
    indexInChg = econVar(usingVariable).index;

    // check the tariff - make sure they match
    if (chargeSimple(indexInChg).namePt != usingVariable) {
        ShowWarningError(state, "UtilityCost:Tariff Debugging issue. ChargeSimple index does not match variable pointer.");
        ShowContinueError(state, "   Between: " + econVar(usingVariable).name);
        ShowContinueError(state, "       And: " + econVar(chargeSimple(indexInChg).namePt).name);
    }
    if (chargeSimple(indexInChg).tariffIndx != curTariff) {
        ShowWarningError(state, "UtilityCost:Tariff Debugging issue. ChargeSimple index does not match tariff index.");
        ShowContinueError(state, "   Between: " + tariff(curTariff).tariffName);
        ShowContinueError(state, "       And: " + tariff(chargeSimple(indexInChg).tariffIndx).tariffName);
    }
    // data from the Charge:Simple
    sourceVals = econVar(chargeSimple(indexInChg).sourcePt).values;
    // determine if costPer should be based on variable or value
    if (chargeSimple(indexInChg).costPerPt != 0) {
        costPer = econVar(chargeSimple(indexInChg).costPerPt).values;
    } else {
        costPer = chargeSimple(indexInChg).costPerVal;
    }
    // find proper season mask
    {
        auto const SELECT_CASE_var(chargeSimple(indexInChg).season);
        if (SELECT_CASE_var == seasonSummer) {
            seasonMask = econVar(tariff(curTariff).nativeIsSummer).values;
        } else if (SELECT_CASE_var == seasonWinter) {
            seasonMask = econVar(tariff(curTariff).nativeIsWinter).values;
        } else if (SELECT_CASE_var == seasonSpring) {
            seasonMask = econVar(tariff(curTariff).nativeIsSpring).values;
        } else if (SELECT_CASE_var == seasonFall) {
            seasonMask = econVar(tariff(curTariff).nativeIsAutumn).values;
        } else if (SELECT_CASE_var == seasonAnnual) {
            seasonMask = 1.0; // all months are 1
        }
    }
    // finally perform calculations
    resultChg = sourceVals * costPer * seasonMask;
    // store the cost in the name of the variable
    econVar(usingVariable).values = resultChg;
    // set the flag that it has been evaluated so it won't be evaluated multiple times
    econVar(usingVariable).isEvaluated = true;
}

void evaluateChargeBlock(EnergyPlusData &state, int const usingVariable)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    int curTariff;
    int indexInChg;
    int iBlk;
    int jMonth;
    Array1D<Real64> sourceVals(MaxNumMonths);
    Array1D<Real64> blkSzMult(MaxNumMonths);
    Array1D<Real64> remainVals(MaxNumMonths);
    Array1D<Real64> resultChg(MaxNumMonths);
    Array1D<Real64> amountForBlk(MaxNumMonths);
    Array1D<Real64> curBlkSz(MaxNumMonths);
    Array1D<Real64> curBlkCost(MaxNumMonths);
    Array1D<Real64> seasonMask(MaxNumMonths);
    bool flagAllZero;

    auto &econVar(state.dataEconTariff->econVar);
    auto &chargeBlock(state.dataEconTariff->chargeBlock);
    auto &tariff(state.dataEconTariff->tariff);

    curTariff = econVar(usingVariable).tariffIndx;
    indexInChg = econVar(usingVariable).index;

    // check the tariff - make sure they match
    if (chargeBlock(indexInChg).namePt != usingVariable) {
        ShowWarningError(state, "UtilityCost:Tariff Debugging issue. chargeBlock index does not match variable pointer.");
        ShowContinueError(state, "   Between: " + econVar(usingVariable).name);
        ShowContinueError(state, "       And: " + econVar(chargeBlock(indexInChg).namePt).name);
    }
    if (chargeBlock(indexInChg).tariffIndx != curTariff) {
        ShowWarningError(state, "UtilityCost:Tariff Debugging issue. chargeBlock index does not match tariff index.");
        ShowContinueError(state, "   Between: " + tariff(curTariff).tariffName);
        ShowContinueError(state, "       And: " + tariff(chargeBlock(indexInChg).tariffIndx).tariffName);
    }
    // data from the chargeBlock
    sourceVals = econVar(chargeBlock(indexInChg).sourcePt).values;
    // find proper season mask
    {
        auto const SELECT_CASE_var(chargeBlock(indexInChg).season);
        if (SELECT_CASE_var == seasonSummer) {
            seasonMask = econVar(tariff(curTariff).nativeIsSummer).values;
        } else if (SELECT_CASE_var == seasonWinter) {
            seasonMask = econVar(tariff(curTariff).nativeIsWinter).values;
        } else if (SELECT_CASE_var == seasonSpring) {
            seasonMask = econVar(tariff(curTariff).nativeIsSpring).values;
        } else if (SELECT_CASE_var == seasonFall) {
            seasonMask = econVar(tariff(curTariff).nativeIsAutumn).values;
        } else if (SELECT_CASE_var == seasonAnnual) {
            seasonMask = 1.0; // all months are 1
        }
    }
    // get block size multiplier
    if (chargeBlock(indexInChg).blkSzMultPt != 0) {
        blkSzMult = econVar(chargeBlock(indexInChg).blkSzMultPt).values;
    } else {
        blkSzMult = chargeBlock(indexInChg).blkSzMultVal;
    }
    // initially set the remaing energy or demand to the source
    remainVals = sourceVals;
    // initially set the result (cost) to zero
    resultChg = 0.0;
    // loop through the blocks performing calculations
    for (iBlk = 1; iBlk <= chargeBlock(indexInChg).numBlk; ++iBlk) {
        if (chargeBlock(indexInChg).blkSzPt(iBlk) != 0) {
            curBlkSz = econVar(chargeBlock(indexInChg).blkSzPt(iBlk)).values;
        } else {
            curBlkSz = chargeBlock(indexInChg).blkSzVal(iBlk);
        }
        if (chargeBlock(indexInChg).blkCostPt(iBlk) != 0) {
            curBlkCost = econVar(chargeBlock(indexInChg).blkCostPt(iBlk)).values;
        } else {
            curBlkCost = chargeBlock(indexInChg).blkCostVal(iBlk);
        }
        // loop through the months
        for (jMonth = 1; jMonth <= MaxNumMonths; ++jMonth) {
            if (seasonMask(jMonth) == 1) {
                // IF ((curBlkSz(jMonth) * blkSzMult(jMonth)) .GT. remainVals(jMonth)) THEN - CR 6547
                if (blkSzMult(jMonth) != 0) {
                    if (curBlkSz(jMonth) > (remainVals(jMonth) / blkSzMult(jMonth))) {
                        amountForBlk(jMonth) = remainVals(jMonth);
                    } else {
                        amountForBlk(jMonth) = curBlkSz(jMonth) * blkSzMult(jMonth);
                    }
                } else {
                    amountForBlk(jMonth) = 0.0;
                }
                resultChg(jMonth) += amountForBlk(jMonth) * curBlkCost(jMonth);
                remainVals(jMonth) -= amountForBlk(jMonth);
            }
        }
    }
    // store the amount remaining if a variable is specified
    if (chargeBlock(indexInChg).remainingPt != 0) {
        econVar(chargeBlock(indexInChg).remainingPt).values = remainVals;
    } else {
        flagAllZero = true;
        for (jMonth = 1; jMonth <= MaxNumMonths; ++jMonth) {
            if (seasonMask(jMonth) == 1) {
                if (remainVals(jMonth) != 0) {
                    flagAllZero = false;
                }
            }
        }
        if (!flagAllZero) {
            ShowWarningError(state, "UtilityCost:Tariff Not all energy or demand was assigned in the block charge: " + econVar(usingVariable).name);
        }
    }
    // store the cost in the name of the variable
    econVar(usingVariable).values = resultChg;
    // set the flag that it has been evaluated so it won't be evaluated multiple times
    econVar(usingVariable).isEvaluated = true;
}

void evaluateRatchet(EnergyPlusData &state, int const usingVariable)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    int curTariff;
    int indexInChg;
    Array1D<Real64> baselineVals(MaxNumMonths);
    Array1D<Real64> adjustmentVals(MaxNumMonths);
    Array1D<Real64> multiplierVals(MaxNumMonths);
    Array1D<Real64> offsetVals(MaxNumMonths);
    Array1D<Real64> seasonFromMask(MaxNumMonths);
    Array1D<Real64> seasonToMask(MaxNumMonths);
    bool isMonthly(false);
    Array1D<Real64> adjSeasonal(MaxNumMonths);
    Array1D<Real64> adjPeak(MaxNumMonths);
    Array1D<Real64> maxAdjBase(MaxNumMonths);
    Real64 maximumVal;
    int iMonth;
    Array1D<Real64> finalResult(MaxNumMonths);

    auto &econVar(state.dataEconTariff->econVar);
    auto &tariff(state.dataEconTariff->tariff);
    auto &ratchet(state.dataEconTariff->ratchet);

    curTariff = econVar(usingVariable).tariffIndx;
    indexInChg = econVar(usingVariable).index;

    // check the tariff - make sure they match
    if (ratchet(indexInChg).namePt != usingVariable) {
        ShowWarningError(state, "UtilityCost:Tariff Debugging issue. Ratchet index does not match variable pointer.");
        ShowContinueError(state, "   Between: " + econVar(usingVariable).name);
        ShowContinueError(state, "       And: " + econVar(ratchet(indexInChg).namePt).name);
    }
    if (ratchet(indexInChg).tariffIndx != curTariff) {
        ShowWarningError(state, "UtilityCost:Tariff Debugging issue. Ratchet index does not match tariff index.");
        ShowContinueError(state, "   Between: " + tariff(curTariff).tariffName);
        ShowContinueError(state, "       And: " + tariff(ratchet(indexInChg).tariffIndx).tariffName);
    }
    // data from the Ratchet
    baselineVals = econVar(ratchet(indexInChg).baselinePt).values;
    adjustmentVals = econVar(ratchet(indexInChg).adjustmentPt).values;
    // determine if multiplier should be based on variable or value
    if (ratchet(indexInChg).multiplierPt != 0) {
        multiplierVals = econVar(ratchet(indexInChg).multiplierPt).values;
    } else {
        multiplierVals = ratchet(indexInChg).multiplierVal;
    }
    // determine if offset should be based on variable or value
    if (ratchet(indexInChg).offsetPt != 0) {
        offsetVals = econVar(ratchet(indexInChg).offsetPt).values;
    } else {
        offsetVals = ratchet(indexInChg).offsetVal;
    }
    // find proper season from mask
    {
        auto const SELECT_CASE_var(ratchet(indexInChg).seasonFrom);
        if (SELECT_CASE_var == seasonSummer) {
            seasonFromMask = econVar(tariff(curTariff).nativeIsSummer).values;
            isMonthly = false;
        } else if (SELECT_CASE_var == seasonWinter) {
            seasonFromMask = econVar(tariff(curTariff).nativeIsWinter).values;
            isMonthly = false;
        } else if (SELECT_CASE_var == seasonSpring) {
            seasonFromMask = econVar(tariff(curTariff).nativeIsSpring).values;
            isMonthly = false;
        } else if (SELECT_CASE_var == seasonFall) {
            seasonFromMask = econVar(tariff(curTariff).nativeIsAutumn).values;
            isMonthly = false;
        } else if (SELECT_CASE_var == seasonAnnual) {
            seasonFromMask = 1.0; // all months are 1
            isMonthly = false;
        } else if (SELECT_CASE_var == seasonMonthly) {
            seasonFromMask = 1.0; // all months are 1
            isMonthly = true;
        } else {
            assert(false);
        }
    }
    // find proper season to mask
    {
        auto const SELECT_CASE_var(ratchet(indexInChg).seasonTo);
        if (SELECT_CASE_var == seasonSummer) {
            seasonToMask = econVar(tariff(curTariff).nativeIsSummer).values;
        } else if (SELECT_CASE_var == seasonWinter) {
            seasonToMask = econVar(tariff(curTariff).nativeIsWinter).values;
        } else if (SELECT_CASE_var == seasonSpring) {
            seasonToMask = econVar(tariff(curTariff).nativeIsSpring).values;
        } else if (SELECT_CASE_var == seasonFall) {
            seasonToMask = econVar(tariff(curTariff).nativeIsAutumn).values;
        } else if (SELECT_CASE_var == seasonAnnual) {
            seasonToMask = 1.0; // all months are 1
        }
    }
    // finally perform calculations
    if (isMonthly) {
        adjSeasonal = adjustmentVals;
    } else {
        maximumVal = -HUGE_(Real64());
        for (iMonth = 1; iMonth <= MaxNumMonths; ++iMonth) {
            if (seasonFromMask(iMonth) == 1) {
                if (adjustmentVals(iMonth) > maximumVal) {
                    maximumVal = adjustmentVals(iMonth);
                }
            }
        }
        adjSeasonal = maximumVal;
    }
    for (iMonth = 1; iMonth <= MaxNumMonths; ++iMonth) {
        // calculate adjusted peak value after offset and multiplier
        adjPeak(iMonth) = (adjSeasonal(iMonth) + offsetVals(iMonth)) * multiplierVals(iMonth);
        // the maximum of the adjustment and the baseline
        if (adjPeak(iMonth) > baselineVals(iMonth)) {
            maxAdjBase(iMonth) = adjPeak(iMonth);
        } else {
            maxAdjBase(iMonth) = baselineVals(iMonth);
        }
    }
    for (iMonth = 1; iMonth <= MaxNumMonths; ++iMonth) {
        if (seasonToMask(iMonth) == 1) {
            finalResult(iMonth) = maxAdjBase(iMonth);
        } else {
            finalResult(iMonth) = baselineVals(iMonth);
        }
    }
    // store the cost in the name of the variable
    econVar(usingVariable).values = finalResult;
    // set the flag that it has been evaluated so it won't be evaluated multiple times
    econVar(usingVariable).isEvaluated = true;
}

void evaluateQualify(EnergyPlusData &state, int const usingVariable)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    int curTariff;
    int indexInQual;
    Array1D<Real64> sourceVals(MaxNumMonths);
    Array1D<Real64> thresholdVals(MaxNumMonths);
    Array1D_int monthsQualify(MaxNumMonths);
    Array1D<Real64> seasonMask(MaxNumMonths);
    bool curIsMaximum;
    bool curIsConsecutive;
    int curNumberOfMonths;
    int adjNumberOfMonths;
    int iMonth;
    bool isQualified;
    int monthsInSeason;
    int cntAllQualMonths;
    int cntConsecQualMonths;
    int maxConsecQualMonths;

    auto &econVar(state.dataEconTariff->econVar);
    auto &qualify(state.dataEconTariff->qualify);
    auto &tariff(state.dataEconTariff->tariff);

    curTariff = econVar(usingVariable).tariffIndx;
    indexInQual = econVar(usingVariable).index;
    // check the tariff - make sure they match
    if (qualify(indexInQual).namePt != usingVariable) {
        ShowWarningError(state, "UtilityCost:Tariff Debugging issue. Qualify index does not match variable pointer.");
        ShowContinueError(state, "   Between: " + econVar(usingVariable).name);
        ShowContinueError(state, "       And: " + econVar(qualify(indexInQual).namePt).name);
    }
    if (qualify(indexInQual).tariffIndx != curTariff) {
        ShowWarningError(state, "UtilityCost:Tariff Debugging issue. Qualify index does not match tariff index.");
        ShowContinueError(state, "   Between: " + tariff(curTariff).tariffName);
        ShowContinueError(state, "       And: " + tariff(qualify(indexInQual).tariffIndx).tariffName);
    }
    // data from the Qualify
    sourceVals = econVar(qualify(indexInQual).sourcePt).values;
    curIsMaximum = qualify(indexInQual).isMaximum;
    curIsConsecutive = qualify(indexInQual).isConsecutive;
    curNumberOfMonths = qualify(indexInQual).numberOfMonths;
    // determine if threshold should be based on variable or value
    if (qualify(indexInQual).thresholdPt != 0) {
        thresholdVals = econVar(qualify(indexInQual).thresholdPt).values;
    } else {
        thresholdVals = qualify(indexInQual).thresholdVal;
    }
    // find proper season mask
    {
        auto const SELECT_CASE_var(qualify(indexInQual).season);
        if (SELECT_CASE_var == seasonSummer) {
            seasonMask = econVar(tariff(curTariff).nativeIsSummer).values;
        } else if (SELECT_CASE_var == seasonWinter) {
            seasonMask = econVar(tariff(curTariff).nativeIsWinter).values;
        } else if (SELECT_CASE_var == seasonSpring) {
            seasonMask = econVar(tariff(curTariff).nativeIsSpring).values;
        } else if (SELECT_CASE_var == seasonFall) {
            seasonMask = econVar(tariff(curTariff).nativeIsAutumn).values;
        } else if (SELECT_CASE_var == seasonAnnual) {
            seasonMask = 1.0; // all months are 1
        }
    }
    // any months with no energy use are excluded from the qualification process
    for (iMonth = 1; iMonth <= MaxNumMonths; ++iMonth) {
        if (econVar(tariff(curTariff).nativeTotalEnergy).values(iMonth) == 0) {
            seasonMask(iMonth) = 0.0;
        }
    }
    // finally perform calculations
    // loop through the months
    monthsInSeason = 0;
    for (iMonth = 1; iMonth <= MaxNumMonths; ++iMonth) {
        if (seasonMask(iMonth) == 1) {
            ++monthsInSeason;
            // use threshold as maximum or minimum
            if (curIsMaximum) {
                if (sourceVals(iMonth) > thresholdVals(iMonth)) {
                    monthsQualify(iMonth) = 0; // greater than maximum threshold so it is not qualified
                } else {
                    monthsQualify(iMonth) = 1; // less than maximum threshold so it is qualified
                }
            } else {
                if (sourceVals(iMonth) < thresholdVals(iMonth)) {
                    monthsQualify(iMonth) = 0; // less than minimum threshold so it is not qualified
                } else {
                    monthsQualify(iMonth) = 1; // greater than minimum threshold so it is qualified
                }
            }
        } else {
            monthsQualify(iMonth) = -1; // flag that indicates not part of the season
        }
    }
    // see if the number of months is longer then the number of months and adjust
    if (curNumberOfMonths > monthsInSeason) {
        adjNumberOfMonths = monthsInSeason;
    } else {
        adjNumberOfMonths = curNumberOfMonths;
    }
    // now that each month is qualified or not, depending on the type of test see if the entire qualify passe or not
    cntAllQualMonths = 0;
    cntConsecQualMonths = 0;
    maxConsecQualMonths = 0;
    for (iMonth = 1; iMonth <= MaxNumMonths; ++iMonth) {
        {
            auto const SELECT_CASE_var(monthsQualify(iMonth));
            if (SELECT_CASE_var == 1) { // qualified
                ++cntAllQualMonths;
                ++cntConsecQualMonths;
                // see if the count is greater then the previous count and if it is make it the new count
                if (cntConsecQualMonths > maxConsecQualMonths) {
                    maxConsecQualMonths = cntConsecQualMonths;
                }
            } else if (SELECT_CASE_var == 0) { // not qualified
                // reset the counter on consecutive months
                cntConsecQualMonths = 0;
            }
        }
    }
    // if test is for consecutive months
    if (curIsConsecutive) {
        if (maxConsecQualMonths >= adjNumberOfMonths) {
            isQualified = true;
        } else {
            isQualified = false;
        }
    } else { // count not consecutive
        if (cntAllQualMonths >= adjNumberOfMonths) {
            isQualified = true;
        } else {
            isQualified = false;
        }
    }
    // now update the tariff level qualifier - only update if the tariff is still qualified
    // and the current qualifer fails.
    if (tariff(curTariff).isQualified) {
        if (!isQualified) {
            tariff(curTariff).isQualified = false;
            tariff(curTariff).ptDisqualifier = usingVariable;
        }
    }
    // store the cost in the name of the variable
    econVar(usingVariable).values = monthsQualify;
    // set the flag that it has been evaluated so it won't be evaluated multiple times
    econVar(usingVariable).isEvaluated = true;
}

void addMonthlyCharge(EnergyPlusData &state, int const usingVariable)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //    Include the monthly charges in the calculations

    int curTariff;

    auto &econVar(state.dataEconTariff->econVar);
    auto &tariff(state.dataEconTariff->tariff);

    curTariff = econVar(usingVariable).tariffIndx;
    // check the tariff - make sure they match
    if (tariff(curTariff).ptServiceCharges != usingVariable) {
        ShowWarningError(state, "UtilityCost:Tariff Debugging issue. Tariff index for service charge does not match variable pointer.");
        ShowContinueError(state, "   Between: " + tariff(curTariff).tariffName);
        ShowContinueError(state, "       And: " + tariff(tariff(curTariff).ptServiceCharges).tariffName);
    }
    if (tariff(curTariff).monthChgPt != 0) {
        econVar(usingVariable).values += econVar(tariff(curTariff).monthChgPt).values;
    } else {
        econVar(usingVariable).values += tariff(curTariff).monthChgVal;
    }
    // zero out months with no energy consumption
    // curTotalEnergy = tariff(curTariff)%nativeTotalEnergy
    // DO iMonth = 1, MaxNumMonths
    //  IF (econVar(curTotalEnergy)%values(iMonth) .EQ. 0) THEN
    //    econVar(usingVariable)%values(iMonth) = 0
    //  END IF
    // END DO
}

void checkMinimumMonthlyCharge(EnergyPlusData &state, int const curTariff)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   August 2008

    //    Check if the total is as big as the minimum monthly charge

    int iMonth;
    int totalVar;
    int minMonVar;

    auto &tariff(state.dataEconTariff->tariff);
    auto &econVar(state.dataEconTariff->econVar);

    totalVar = tariff(curTariff).ptTotal;
    minMonVar = tariff(curTariff).minMonthChgPt;
    // if a variable is defined use that
    if (minMonVar != 0) {
        for (iMonth = 1; iMonth <= MaxNumMonths; ++iMonth) {
            if (econVar(totalVar).values(iMonth) < econVar(minMonVar).values(iMonth)) {
                econVar(totalVar).values(iMonth) = econVar(minMonVar).values(iMonth);
            }
        }
    } else { // use the constant value
        for (iMonth = 1; iMonth <= MaxNumMonths; ++iMonth) {
            if (econVar(totalVar).values(iMonth) < tariff(curTariff).minMonthChgVal) {
                econVar(totalVar).values(iMonth) = tariff(curTariff).minMonthChgVal;
            }
        }
    }
}

void setNativeVariables(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //    Set up the "built in" i.e. native variables that hold
    //    the energy and demand from the simulation.

    int iTariff;
    int jPeriod;
    int kMonth;
    Array1D<Real64> monthVal(MaxNumMonths);
    Real64 bigNumber(0.0); // Autodesk Value not used but suppresses warning about HUGE_() call

    auto &tariff(state.dataEconTariff->tariff);
    auto &econVar(state.dataEconTariff->econVar);

    bigNumber = HUGE_(bigNumber);
    for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
        // nativeTotalEnergy
        monthVal = 0.0;
        for (jPeriod = 1; jPeriod <= countPeriod; ++jPeriod) {
            for (kMonth = 1; kMonth <= MaxNumMonths; ++kMonth) {
                monthVal(kMonth) += tariff(iTariff).gatherEnergy(kMonth, jPeriod);
            }
        }
        econVar(tariff(iTariff).nativeTotalEnergy).values = monthVal;
        // nativeTotalDemand
        monthVal = -bigNumber;
        for (jPeriod = 1; jPeriod <= countPeriod; ++jPeriod) {
            for (kMonth = 1; kMonth <= MaxNumMonths; ++kMonth) {
                if (tariff(iTariff).gatherDemand(kMonth, jPeriod) > monthVal(kMonth)) {
                    monthVal(kMonth) = tariff(iTariff).gatherDemand(kMonth, jPeriod);
                }
            }
        }
        // if no maximum was set just set to zero
        for (kMonth = 1; kMonth <= MaxNumMonths; ++kMonth) {
            if (monthVal(kMonth) == -bigNumber) {
                monthVal(kMonth) = 0.0;
            }
        }
        econVar(tariff(iTariff).nativeTotalDemand).values = monthVal;
        for (kMonth = 1; kMonth <= MaxNumMonths; ++kMonth) {
            // nativePeakEnergy
            econVar(tariff(iTariff).nativePeakEnergy).values(kMonth) = tariff(iTariff).gatherEnergy(kMonth, periodPeak);
            // nativePeakDemand
            econVar(tariff(iTariff).nativePeakDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodPeak);
            // nativeShoulderEnergy
            econVar(tariff(iTariff).nativeShoulderEnergy).values(kMonth) = tariff(iTariff).gatherEnergy(kMonth, periodShoulder);
            // nativeShoulderDemand
            econVar(tariff(iTariff).nativeShoulderDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodShoulder);
            // nativeOffPeakEnergy
            econVar(tariff(iTariff).nativeOffPeakEnergy).values(kMonth) = tariff(iTariff).gatherEnergy(kMonth, periodOffPeak);
            // nativeOffPeakDemand
            econVar(tariff(iTariff).nativeOffPeakDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodOffPeak);
            // nativeMidPeakEnergy
            econVar(tariff(iTariff).nativeMidPeakEnergy).values(kMonth) = tariff(iTariff).gatherEnergy(kMonth, periodMidPeak);
            // nativeMidPeakDemand
            econVar(tariff(iTariff).nativeMidPeakDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodMidPeak);
            // nativePeakExceedsOffPeak
            monthVal(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodPeak) - tariff(iTariff).gatherDemand(kMonth, periodOffPeak);
            if (monthVal(kMonth) > 0) {
                econVar(tariff(iTariff).nativePeakExceedsOffPeak).values(kMonth) = monthVal(kMonth);
            } else {
                econVar(tariff(iTariff).nativePeakExceedsOffPeak).values(kMonth) = 0.0;
            }
            // nativeOffPeakExceedsPeak
            monthVal(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodOffPeak) - tariff(iTariff).gatherDemand(kMonth, periodPeak);
            if (monthVal(kMonth) > 0) {
                econVar(tariff(iTariff).nativeOffPeakExceedsPeak).values(kMonth) = monthVal(kMonth);
            } else {
                econVar(tariff(iTariff).nativeOffPeakExceedsPeak).values(kMonth) = 0.0;
            }
            // nativePeakExceedsMidPeak
            monthVal(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodPeak) - tariff(iTariff).gatherDemand(kMonth, periodMidPeak);
            if (monthVal(kMonth) > 0) {
                econVar(tariff(iTariff).nativePeakExceedsMidPeak).values(kMonth) = monthVal(kMonth);
            } else {
                econVar(tariff(iTariff).nativePeakExceedsOffPeak).values(kMonth) = 0.0;
            }
            // nativeMidPeakExceedsPeak
            monthVal(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodMidPeak) - tariff(iTariff).gatherDemand(kMonth, periodPeak);
            if (monthVal(kMonth) > 0) {
                econVar(tariff(iTariff).nativeMidPeakExceedsPeak).values(kMonth) = monthVal(kMonth);
            } else {
                econVar(tariff(iTariff).nativeMidPeakExceedsPeak).values(kMonth) = 0.0;
            }
            // nativePeakExceedsShoulder
            monthVal(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodPeak) - tariff(iTariff).gatherDemand(kMonth, periodShoulder);
            if (monthVal(kMonth) > 0) {
                econVar(tariff(iTariff).nativePeakExceedsShoulder).values(kMonth) = monthVal(kMonth);
            } else {
                econVar(tariff(iTariff).nativePeakExceedsShoulder).values(kMonth) = 0.0;
            }
            // nativeShoulderExceedsPeak
            monthVal(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodShoulder) - tariff(iTariff).gatherDemand(kMonth, periodPeak);
            if (monthVal(kMonth) > 0) {
                econVar(tariff(iTariff).nativeShoulderExceedsPeak).values(kMonth) = monthVal(kMonth);
            } else {
                econVar(tariff(iTariff).nativeShoulderExceedsPeak).values(kMonth) = 0.0;
            }
            // nativeIsWinter
            // nativeIsNotWinter
            if (tariff(iTariff).seasonForMonth(kMonth) == seasonWinter) {
                econVar(tariff(iTariff).nativeIsWinter).values(kMonth) = 1.0;
                econVar(tariff(iTariff).nativeIsNotWinter).values(kMonth) = 0.0;
            } else {
                econVar(tariff(iTariff).nativeIsWinter).values(kMonth) = 0.0;
                econVar(tariff(iTariff).nativeIsNotWinter).values(kMonth) = 1.0;
            }
            // nativeIsSpring
            // nativeIsNotSpring
            if (tariff(iTariff).seasonForMonth(kMonth) == seasonSpring) {
                econVar(tariff(iTariff).nativeIsSpring).values(kMonth) = 1.0;
                econVar(tariff(iTariff).nativeIsNotSpring).values(kMonth) = 0.0;
            } else {
                econVar(tariff(iTariff).nativeIsSpring).values(kMonth) = 0.0;
                econVar(tariff(iTariff).nativeIsNotSpring).values(kMonth) = 1.0;
            }
            // nativeIsSummer
            // nativeIsNotSummer
            if (tariff(iTariff).seasonForMonth(kMonth) == seasonSummer) {
                econVar(tariff(iTariff).nativeIsSummer).values(kMonth) = 1.0;
                econVar(tariff(iTariff).nativeIsNotSummer).values(kMonth) = 0.0;
            } else {
                econVar(tariff(iTariff).nativeIsSummer).values(kMonth) = 0.0;
                econVar(tariff(iTariff).nativeIsNotSummer).values(kMonth) = 1.0;
            }
            // nativeIsAutumn
            // nativeIsNotAutumn
            if (tariff(iTariff).seasonForMonth(kMonth) == seasonFall) {
                econVar(tariff(iTariff).nativeIsAutumn).values(kMonth) = 1.0;
                econVar(tariff(iTariff).nativeIsNotAutumn).values(kMonth) = 0.0;
            } else {
                econVar(tariff(iTariff).nativeIsAutumn).values(kMonth) = 0.0;
                econVar(tariff(iTariff).nativeIsNotAutumn).values(kMonth) = 1.0;
            }
            // nativePeakAndShoulderEnergy
            econVar(tariff(iTariff).nativePeakAndShoulderEnergy).values(kMonth) =
                tariff(iTariff).gatherEnergy(kMonth, periodPeak) + tariff(iTariff).gatherEnergy(kMonth, periodShoulder);
            // nativePeakAndShoulderDemand
            if (tariff(iTariff).gatherDemand(kMonth, periodPeak) > tariff(iTariff).gatherDemand(kMonth, periodShoulder)) {
                econVar(tariff(iTariff).nativePeakAndShoulderDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodPeak);
            } else {
                econVar(tariff(iTariff).nativePeakAndShoulderDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodShoulder);
            }
            // nativePeakAndMidPeakEnergy
            econVar(tariff(iTariff).nativePeakAndMidPeakEnergy).values(kMonth) =
                tariff(iTariff).gatherEnergy(kMonth, periodPeak) + tariff(iTariff).gatherEnergy(kMonth, periodMidPeak);
            // nativePeakAndMidPeakDemand
            if (tariff(iTariff).gatherDemand(kMonth, periodPeak) > tariff(iTariff).gatherDemand(kMonth, periodMidPeak)) {
                econVar(tariff(iTariff).nativePeakAndMidPeakDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodPeak);
            } else {
                econVar(tariff(iTariff).nativePeakAndMidPeakDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodMidPeak);
            }
            // nativeShoulderAndOffPeakEnergy
            econVar(tariff(iTariff).nativeShoulderAndOffPeakEnergy).values(kMonth) =
                tariff(iTariff).gatherEnergy(kMonth, periodShoulder) + tariff(iTariff).gatherEnergy(kMonth, periodOffPeak);
            // nativeShoulderAndOffPeakDemand
            if (tariff(iTariff).gatherDemand(kMonth, periodShoulder) > tariff(iTariff).gatherDemand(kMonth, periodOffPeak)) {
                econVar(tariff(iTariff).nativeShoulderAndOffPeakDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodShoulder);
            } else {
                econVar(tariff(iTariff).nativeShoulderAndOffPeakDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodOffPeak);
            }
            // nativePeakAndOffPeakEnergy
            econVar(tariff(iTariff).nativePeakAndOffPeakEnergy).values(kMonth) =
                tariff(iTariff).gatherEnergy(kMonth, periodPeak) + tariff(iTariff).gatherEnergy(kMonth, periodOffPeak);
            // nativePeakAndOffPeakDemand
            if (tariff(iTariff).gatherDemand(kMonth, periodPeak) > tariff(iTariff).gatherDemand(kMonth, periodOffPeak)) {
                econVar(tariff(iTariff).nativePeakAndOffPeakDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodPeak);
            } else {
                econVar(tariff(iTariff).nativePeakAndOffPeakDemand).values(kMonth) = tariff(iTariff).gatherDemand(kMonth, periodOffPeak);
            }
            // nativeRealTimePriceCosts
            econVar(tariff(iTariff).nativeRealTimePriceCosts).values(kMonth) = tariff(iTariff).RTPcost(kMonth);
            // nativeAboveCustomerBaseCosts
            econVar(tariff(iTariff).nativeAboveCustomerBaseCosts).values(kMonth) = tariff(iTariff).RTPaboveBaseCost(kMonth);
            // nativeBelowCustomerBaseCosts
            econVar(tariff(iTariff).nativeBelowCustomerBaseCosts).values(kMonth) = tariff(iTariff).RTPbelowBaseCost(kMonth);
            // nativeAboveCustomerBaseEnergy
            econVar(tariff(iTariff).nativeAboveCustomerBaseEnergy).values(kMonth) = tariff(iTariff).RTPaboveBaseEnergy(kMonth);
            // nativeBelowCustomerBaseEnergy
            econVar(tariff(iTariff).nativeBelowCustomerBaseEnergy).values(kMonth) = tariff(iTariff).RTPbelowBaseEnergy(kMonth);
        }
    }
}

void LEEDtariffReporting(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   October 2012

    //    Write the economic results for LEED reporting

    using namespace OutputReportPredefined;

    int distCoolFacilMeter;
    int distHeatFacilMeter;
    Real64 elecTotalEne;
    Real64 gasTotalEne;
    Real64 distCoolTotalEne;
    Real64 distHeatTotalEne;
    Real64 otherTotalEne;
    Real64 elecTotalCost;
    Real64 gasTotalCost;
    Real64 otherTotalCost;
    Real64 distCoolTotalCost;
    Real64 distHeatTotalCost;
    Real64 allTotalCost;
    std::string elecTariffNames;
    std::string gasTariffNames;
    std::string distCoolTariffNames;
    std::string distHeatTariffNames;
    std::string othrTariffNames;
    iEconConv elecUnits;
    iEconConv gasUnits;
    iEconConv distCoolUnits;
    iEconConv distHeatUnits;
    iEconConv othrUnits;
    iDemandWindow gasDemWindowUnits;
    iDemandWindow distCoolDemWindowUnits;
    iDemandWindow distHeatDemWindowUnits;
    iDemandWindow othrDemWindowUnits;
    int iTariff;

    auto &tariff(state.dataEconTariff->tariff);

    if (state.dataEconTariff->numTariff > 0) {
        distCoolFacilMeter = GetMeterIndex(state, "DISTRICTCOOLING:FACILITY");
        distHeatFacilMeter = GetMeterIndex(state, "DISTRICTHEATING:FACILITY");
        elecTotalEne = 0.0;
        gasTotalEne = 0.0;
        distCoolTotalEne = 0.0;
        distHeatTotalEne = 0.0;
        otherTotalEne = 0.0;
        elecTotalCost = 0.0;
        gasTotalCost = 0.0;
        distCoolTotalCost = 0.0;
        distHeatTotalCost = 0.0;
        otherTotalCost = 0.0;
        allTotalCost = 0.0;
        elecUnits = iEconConv::USERDEF;
        gasUnits = iEconConv::USERDEF;
        distCoolUnits = iEconConv::USERDEF;
        distHeatUnits = iEconConv::USERDEF;
        othrUnits = iEconConv::USERDEF;
        gasDemWindowUnits = iDemandWindow::Unassigned;
        othrDemWindowUnits = iDemandWindow::Unassigned;
        elecTariffNames = "";
        gasTariffNames = "";
        distCoolTariffNames = "";
        distHeatTariffNames = "";
        othrTariffNames = "";
        for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
            if (tariff(iTariff).isSelected) {
                allTotalCost += tariff(iTariff).totalAnnualCost;
                if (tariff(iTariff).kindElectricMtr >= kindMeterElecSimple) {
                    if (tariff(iTariff).totalAnnualEnergy > elecTotalEne) elecTotalEne = tariff(iTariff).totalAnnualEnergy;
                    elecTotalCost += tariff(iTariff).totalAnnualCost;
                    elecTariffNames += ' ' + tariff(iTariff).tariffName;
                    elecUnits = tariff(iTariff).convChoice;
                } else if (tariff(iTariff).kindGasMtr == kindMeterGas) {
                    if (tariff(iTariff).totalAnnualEnergy > gasTotalEne) gasTotalEne = tariff(iTariff).totalAnnualEnergy;
                    gasTotalCost += tariff(iTariff).totalAnnualCost;
                    gasTariffNames += ' ' + tariff(iTariff).tariffName;
                    gasUnits = tariff(iTariff).convChoice;
                    gasDemWindowUnits = tariff(iTariff).demandWindow;
                } else if (tariff(iTariff).reportMeterIndx == distCoolFacilMeter) {
                    if (tariff(iTariff).totalAnnualEnergy > distCoolTotalEne) distCoolTotalEne = tariff(iTariff).totalAnnualEnergy;
                    distCoolTotalCost += tariff(iTariff).totalAnnualCost;
                    distCoolTariffNames += ' ' + tariff(iTariff).tariffName;
                    distCoolUnits = tariff(iTariff).convChoice;
                    distCoolDemWindowUnits = tariff(iTariff).demandWindow;
                } else if (tariff(iTariff).reportMeterIndx == distHeatFacilMeter) {
                    if (tariff(iTariff).totalAnnualEnergy > distHeatTotalEne) distHeatTotalEne = tariff(iTariff).totalAnnualEnergy;
                    distHeatTotalCost += tariff(iTariff).totalAnnualCost;
                    distHeatTariffNames += ' ' + tariff(iTariff).tariffName;
                    distHeatUnits = tariff(iTariff).convChoice;
                    distHeatDemWindowUnits = tariff(iTariff).demandWindow;
                } else if (tariff(iTariff).kindWaterMtr == kindMeterNotWater) {
                    if (tariff(iTariff).totalAnnualEnergy > otherTotalEne) otherTotalEne = tariff(iTariff).totalAnnualEnergy;
                    otherTotalCost += tariff(iTariff).totalAnnualCost;
                    othrTariffNames += ' ' + tariff(iTariff).tariffName;
                    othrUnits = tariff(iTariff).convChoice;
                    othrDemWindowUnits = tariff(iTariff).demandWindow;
                } else {
                }
            }
        }
        // names of the rates
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsRtNm, "Electricity", elecTariffNames);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsRtNm, "Natural Gas", gasTariffNames);
        if (distCoolTotalEne != 0) PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsRtNm, "District Cooling", distCoolTariffNames);
        if (distHeatTotalEne != 0) PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsRtNm, "District Heating", distHeatTariffNames);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsRtNm, "Other", othrTariffNames);
        // virtual rate
        if (elecTotalEne != 0) PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsVirt, "Electricity", elecTotalCost / elecTotalEne, 3);
        if (gasTotalEne != 0) PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsVirt, "Natural Gas", gasTotalCost / gasTotalEne, 3);
        if (otherTotalEne != 0) PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsVirt, "Other", otherTotalCost / otherTotalEne, 3);
        // units
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsEneUnt, "Electricity", format("{}", convEneStrings(elecUnits)));
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsEneUnt, "Natural Gas", format("{}", convEneStrings(gasUnits)));
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsEneUnt, "Other", format("{}", convEneStrings(othrUnits)));
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsDemUnt, "Electricity", format("{}", convDemStrings(elecUnits)));
        PreDefTableEntry(state,
                         state.dataOutRptPredefined->pdchLeedEtsDemUnt,
                         "Natural Gas",
                         format("{}{}", convDemStrings(gasUnits), demWindowStrings(gasDemWindowUnits)));
        PreDefTableEntry(state,
                         state.dataOutRptPredefined->pdchLeedEtsDemUnt,
                         "Other",
                         format("{}{}", convDemStrings(othrUnits), demWindowStrings(othrDemWindowUnits)));
        // total cost
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEcsTotal, "Electricity", elecTotalCost, 2);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEcsTotal, "Natural Gas", gasTotalCost, 2);
        PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEcsTotal, "Other", otherTotalCost, 2);
        // show district energy if used
        if (distCoolTotalEne != 0) {
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsVirt, "District Cooling", distCoolTotalCost / distCoolTotalEne, 3);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsEneUnt, "District Cooling", format("{}", convEneStrings(distCoolUnits)));
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchLeedEtsDemUnt,
                             "District Cooling",
                             format("{}{}", convDemStrings(distCoolUnits), demWindowStrings(distCoolDemWindowUnits)));
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEcsTotal, "District Cooling", distCoolTotalCost, 2);
        }
        if (distHeatTotalEne != 0) {
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsVirt, "District Heating", distHeatTotalCost / distHeatTotalEne, 3);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEtsEneUnt, "District Heating", format("{}", convEneStrings(distHeatUnits)));
            PreDefTableEntry(state,
                             state.dataOutRptPredefined->pdchLeedEtsDemUnt,
                             "District Heating",
                             format("{}{}", convDemStrings(distHeatUnits), demWindowStrings(distHeatDemWindowUnits)));
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchLeedEcsTotal, "District Heating", distHeatTotalCost, 2);
        }
        // save the total costs for later to compute process fraction
        state.dataOutRptPredefined->LEEDelecCostTotal = elecTotalCost;
        state.dataOutRptPredefined->LEEDgasCostTotal = gasTotalCost;
        state.dataOutRptPredefined->LEEDothrCostTotal = distCoolTotalCost + distHeatTotalCost + otherTotalCost;
        PreDefTableEntry(state,
                         state.dataOutRptPredefined->pdchLeedEcsTotal,
                         "Total",
                         elecTotalCost + gasTotalCost + distCoolTotalCost + distHeatTotalCost + otherTotalCost,
                         2);
    }
}

void WriteTabularTariffReports(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004
    //    MODIFIED       January 2010, Kyle Benne
    //                   Added SQLite output

    using OutputReportTabular::ConvertIP;
    using OutputReportTabular::DetermineBuildingFloorArea;
    using OutputReportTabular::LookupSItoIP;
    using OutputReportTabular::RealToStr;
    using OutputReportTabular::WriteReportHeaders;
    using OutputReportTabular::WriteSubtitle;
    using OutputReportTabular::WriteTable;
    using OutputReportTabular::WriteTextLine;

    // all arrays are in the format: (row, column)
    Array1D_string columnHead;
    Array1D_int columnWidth;
    Array1D_string rowHead;
    Array2D_string tableBody;
    // other local variables
    Real64 elecTotalCost;
    Real64 gasTotalCost;
    Real64 otherTotalCost;
    Real64 allTotalCost;
    std::string outString; // an arbitarilty long string
    int curStep;
    int indexInChg;
    int iTariff;
    int kVar;
    int lStep;
    std::string SIunit;
    int unitConvIndex(0);
    Real64 perAreaUnitConv(0.0);
    std::string perAreaUnitName;

    auto &tariff(state.dataEconTariff->tariff);
    auto &econVar(state.dataEconTariff->econVar);
    auto &computation(state.dataEconTariff->computation);
    auto &chargeSimple(state.dataEconTariff->chargeSimple);
    auto &chargeBlock(state.dataEconTariff->chargeBlock);

    // Here to it is ready to assign ort->unitStyle_SQLite (not in SQLiteProcedures.cc)
    // when ort->unitsStyle inputs should have been concretely processed and assigned.
    // Included this here to make sure the units specifications are correctly updated.
    if (state.dataOutRptTab->unitsStyle_SQLite == OutputReportTabular::iUnitsStyle::NotFound) {
        state.dataOutRptTab->unitsStyle_SQLite = state.dataOutRptTab->unitsStyle; // This is the default UseOutputControlTableStyles
    }

    // compute floor area if no ABUPS
    if (state.dataOutRptTab->buildingConditionedFloorArea == 0.0) {
        DetermineBuildingFloorArea(state);
    }

    if (state.dataEconTariff->numTariff > 0) {
        if (state.dataOutRptTab->displayEconomicResultSummary) {
            DisplayString(state, "Writing Tariff Reports");
            for (auto &e : econVar)
                e.isReported = false;
            showWarningsBasedOnTotal(state);
            //---------------------------------
            // Economics Results Summary Report
            //---------------------------------
            WriteReportHeaders(state, "Economics Results Summary Report", "Entire Facility", OutputProcessor::StoreType::Averaged);

            for (int iUnitSystem = 0; iUnitSystem <= 1; iUnitSystem++) {
                OutputReportTabular::iUnitsStyle unitsStyle_cur = state.dataOutRptTab->unitsStyle;
                bool produceTabular = true;
                bool produceSQLite = false;
                if (produceDualUnitsFlags(iUnitSystem,
                                          state.dataOutRptTab->unitsStyle,
                                          state.dataOutRptTab->unitsStyle_SQLite,
                                          unitsStyle_cur,
                                          produceTabular,
                                          produceSQLite))
                    break;

                // do unit conversions if necessary
                if (unitsStyle_cur == OutputReportTabular::iUnitsStyle::InchPound) {
                    SIunit = "[~~$~~/m2]";
                    LookupSItoIP(state, SIunit, unitConvIndex, perAreaUnitName);
                    perAreaUnitConv = ConvertIP(state, unitConvIndex, 1.0);
                } else {
                    perAreaUnitName = "[~~$~~/m2]";
                    perAreaUnitConv = 1.0;
                }

                //---- Annual Summary
                rowHead.allocate(3);
                columnHead.allocate(4);
                columnWidth.allocate(4);
                tableBody.allocate(4, 3);
                tableBody = "";
                columnHead(1) = "Electricity";
                columnHead(2) = "Natural Gas";
                columnHead(3) = "Other";
                columnHead(4) = "Total";
                rowHead(1) = "Cost [~~$~~]";
                rowHead(2) = "Cost per Total Building Area " + perAreaUnitName;
                rowHead(3) = "Cost per Net Conditioned Building Area " + perAreaUnitName;
                elecTotalCost = 0.0;
                gasTotalCost = 0.0;
                otherTotalCost = 0.0;
                allTotalCost = 0.0;
                for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
                    if (tariff(iTariff).isSelected) {
                        allTotalCost += tariff(iTariff).totalAnnualCost;
                        if (tariff(iTariff).kindElectricMtr >= kindMeterElecSimple) {
                            elecTotalCost += tariff(iTariff).totalAnnualCost;
                        } else if (tariff(iTariff).kindGasMtr == kindMeterGas) {
                            gasTotalCost += tariff(iTariff).totalAnnualCost;
                        } else if (tariff(iTariff).kindWaterMtr == kindMeterNotWater) {
                            otherTotalCost += tariff(iTariff).totalAnnualCost;
                            // removed because this was confusing        columnHead(3) = tariff(iTariff)%reportMeter
                        } else {
                        }
                    }
                }
                tableBody(1, 1) = RealToStr(elecTotalCost, 2);
                tableBody(2, 1) = RealToStr(gasTotalCost, 2);
                tableBody(3, 1) = RealToStr(otherTotalCost, 2);
                tableBody(4, 1) = RealToStr(allTotalCost, 2);
                if (state.dataOutRptTab->buildingGrossFloorArea > 0.0) {
                    tableBody(1, 2) = RealToStr((elecTotalCost / state.dataOutRptTab->buildingGrossFloorArea) * perAreaUnitConv, 2);
                    tableBody(2, 2) = RealToStr((gasTotalCost / state.dataOutRptTab->buildingGrossFloorArea) * perAreaUnitConv, 2);
                    tableBody(3, 2) = RealToStr((otherTotalCost / state.dataOutRptTab->buildingGrossFloorArea) * perAreaUnitConv, 2);
                    tableBody(4, 2) = RealToStr((allTotalCost / state.dataOutRptTab->buildingGrossFloorArea) * perAreaUnitConv, 2);
                }
                if (state.dataOutRptTab->buildingConditionedFloorArea > 0.0) {
                    tableBody(1, 3) = RealToStr((elecTotalCost / state.dataOutRptTab->buildingConditionedFloorArea) * perAreaUnitConv, 2);
                    tableBody(2, 3) = RealToStr((gasTotalCost / state.dataOutRptTab->buildingConditionedFloorArea) * perAreaUnitConv, 2);
                    tableBody(3, 3) = RealToStr((otherTotalCost / state.dataOutRptTab->buildingConditionedFloorArea) * perAreaUnitConv, 2);
                    tableBody(4, 3) = RealToStr((allTotalCost / state.dataOutRptTab->buildingConditionedFloorArea) * perAreaUnitConv, 2);
                }
                columnWidth = 14; // array assignment - same for all columns
                if (produceTabular) {
                    WriteSubtitle(state, "Annual Cost");
                    WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
                }
                if (produceSQLite) {
                    if (state.dataSQLiteProcedures->sqlite) {
                        state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                            tableBody, rowHead, columnHead, "Economics Results Summary Report", "Entire Facility", "Annual Cost");
                    }
                }
                if (produceTabular) {
                    if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
                        state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                            tableBody, rowHead, columnHead, "Economics Results Summary Report", "Entire Facility", "Annual Cost");
                    }
                }
                columnHead.deallocate();
                rowHead.deallocate();
                columnWidth.deallocate();
                tableBody.deallocate();
            }
            //---- Tariff Summary
            rowHead.allocate(state.dataEconTariff->numTariff);
            columnHead.allocate(6);
            columnWidth.allocate(6);
            tableBody.allocate(6, state.dataEconTariff->numTariff);
            tableBody = "";
            columnHead(1) = "Selected";
            columnHead(2) = "Qualified";
            columnHead(3) = "Meter";
            columnHead(4) = "Buy or Sell";
            columnHead(5) = "Group";
            columnHead(6) = "Annual Cost (~~$~~)";
            for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
                rowHead(iTariff) = tariff(iTariff).tariffName;
                if (tariff(iTariff).isSelected) {
                    tableBody(1, iTariff) = "Yes";
                } else {
                    tableBody(1, iTariff) = "No";
                }
                if (tariff(iTariff).isQualified) {
                    tableBody(2, iTariff) = "Yes";
                } else {
                    tableBody(2, iTariff) = "No";
                }
                tableBody(3, iTariff) = tariff(iTariff).reportMeter;
                {
                    auto const SELECT_CASE_var(tariff(iTariff).buyOrSell);
                    if (SELECT_CASE_var == buyFromUtility) {
                        tableBody(4, iTariff) = "Buy";
                    } else if (SELECT_CASE_var == sellToUtility) {
                        tableBody(4, iTariff) = "Sell";
                    } else if (SELECT_CASE_var == netMetering) {
                        tableBody(4, iTariff) = "Net";
                    }
                }
                if (tariff(iTariff).groupName == "") {
                    tableBody(5, iTariff) = "(none)";
                } else {
                    tableBody(5, iTariff) = tariff(iTariff).groupName;
                }
                tableBody(6, iTariff) = RealToStr(tariff(iTariff).totalAnnualCost, 2);
            }
            columnWidth = 14; // array assignment - same for all columns
            WriteSubtitle(state, "Tariff Summary");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                    tableBody, rowHead, columnHead, "Economics Results Summary Report", "Entire Facility", "Tariff Summary");
            }
            if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
                state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                    tableBody, rowHead, columnHead, "Economics Results Summary Report", "Entire Facility", "Tariff Summary");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
        }
        //---------------------------------
        // Tariff Report
        //---------------------------------
        if (state.dataOutRptTab->displayTariffReport) {
            for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
                WriteReportHeaders(state, "Tariff Report", tariff(iTariff).tariffName, OutputProcessor::StoreType::Averaged);
                rowHead.allocate(7);
                columnHead.allocate(1);
                columnWidth.allocate(1);
                tableBody.allocate(1, 7);
                tableBody = "";
                columnHead(1) = "Parameter";
                rowHead(1) = "Meter";
                rowHead(2) = "Selected";
                rowHead(3) = "Group";
                rowHead(4) = "Qualified";
                rowHead(5) = "Disqualifier";
                rowHead(6) = "Computation";
                rowHead(7) = "Units";
                tableBody(1, 1) = tariff(iTariff).reportMeter;
                if (tariff(iTariff).isSelected) {
                    tableBody(1, 2) = "Yes";
                } else {
                    tableBody(1, 2) = "No";
                }
                if (tariff(iTariff).groupName == "") {
                    tableBody(1, 3) = "(none)";
                } else {
                    tableBody(1, 3) = tariff(iTariff).groupName;
                }
                if (tariff(iTariff).isQualified) {
                    tableBody(1, 4) = "Yes";
                } else {
                    tableBody(1, 4) = "No";
                }
                if (tariff(iTariff).isQualified) {
                    tableBody(1, 5) = "n/a";
                } else {
                    tableBody(1, 5) = econVar(tariff(iTariff).ptDisqualifier).name;
                }
                if (computation(iTariff).isUserDef) {
                    tableBody(1, 6) = computation(iTariff).computeName;
                } else {
                    tableBody(1, 6) = "automatic";
                }
                {
                    auto const SELECT_CASE_var(tariff(iTariff).convChoice);
                    if (SELECT_CASE_var == iEconConv::USERDEF) {
                        tableBody(1, 7) = "User Defined";
                    } else if (SELECT_CASE_var == iEconConv::KWH) {
                        tableBody(1, 7) = "kWh";
                    } else if (SELECT_CASE_var == iEconConv::THERM) {
                        tableBody(1, 7) = "Therm";
                    } else if (SELECT_CASE_var == iEconConv::MMBTU) {
                        tableBody(1, 7) = "MMBtu";
                    } else if (SELECT_CASE_var == iEconConv::MJ) {
                        tableBody(1, 7) = "MJ";
                    } else if (SELECT_CASE_var == iEconConv::KBTU) {
                        tableBody(1, 7) = "kBtu";
                    } else if (SELECT_CASE_var == iEconConv::MCF) {
                        tableBody(1, 7) = "MCF";
                    } else if (SELECT_CASE_var == iEconConv::CCF) {
                        tableBody(1, 7) = "CCF";
                    }
                }
                columnWidth = 14; // array assignment - same for all columns
                WriteSubtitle(state, "General");
                WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
                if (state.dataSQLiteProcedures->sqlite) {
                    state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                        tableBody, rowHead, columnHead, "Tariff Report", tariff(iTariff).tariffName, "General");
                }
                if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
                    state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                        tableBody, rowHead, columnHead, "Tariff Report", tariff(iTariff).tariffName, "General");
                }
                columnHead.deallocate();
                rowHead.deallocate();
                columnWidth.deallocate();
                tableBody.deallocate();
                //---- Categories
                for (auto &e : econVar)
                    e.activeNow = false;
                econVar(tariff(iTariff).ptEnergyCharges).activeNow = true;
                econVar(tariff(iTariff).ptDemandCharges).activeNow = true;
                econVar(tariff(iTariff).ptServiceCharges).activeNow = true;
                econVar(tariff(iTariff).ptBasis).activeNow = true;
                econVar(tariff(iTariff).ptAdjustment).activeNow = true;
                econVar(tariff(iTariff).ptSurcharge).activeNow = true;
                econVar(tariff(iTariff).ptSubtotal).activeNow = true;
                econVar(tariff(iTariff).ptTaxes).activeNow = true;
                econVar(tariff(iTariff).ptTotal).activeNow = true;
                ReportEconomicVariable(state, "Categories", false, true, tariff(iTariff).tariffName);
                //---- Charges
                for (auto &e : econVar)
                    e.activeNow = false;
                for (kVar = 1; kVar <= state.dataEconTariff->numEconVar; ++kVar) {
                    if (econVar(kVar).tariffIndx == iTariff) {
                        if ((econVar(kVar).kindOfObj == iEconVarObjType::ChargeSimple) || (econVar(kVar).kindOfObj == iEconVarObjType::ChargeBlock)) {
                            econVar(kVar).activeNow = true;
                        }
                    }
                }
                ReportEconomicVariable(state, "Charges", true, true, tariff(iTariff).tariffName);
                //---- Sources for Charges
                for (auto &e : econVar)
                    e.activeNow = false;
                for (kVar = 1; kVar <= state.dataEconTariff->numEconVar; ++kVar) {
                    if (econVar(kVar).tariffIndx == iTariff) {
                        indexInChg = econVar(kVar).index;
                        if (econVar(kVar).kindOfObj == iEconVarObjType::ChargeSimple) {
                            if (chargeSimple(indexInChg).sourcePt > 0) {
                                econVar(chargeSimple(indexInChg).sourcePt).activeNow = true;
                            }
                        } else if (econVar(kVar).kindOfObj == iEconVarObjType::ChargeBlock) {
                            if (chargeBlock(indexInChg).sourcePt > 0) {
                                econVar(chargeBlock(indexInChg).sourcePt).activeNow = true;
                            }
                        }
                    }
                }
                ReportEconomicVariable(state, "Corresponding Sources for Charges", false, false, tariff(iTariff).tariffName);
                //---- Rachets
                for (auto &e : econVar)
                    e.activeNow = false;
                for (kVar = 1; kVar <= state.dataEconTariff->numEconVar; ++kVar) {
                    if (econVar(kVar).tariffIndx == iTariff) {
                        if (econVar(kVar).kindOfObj == iEconVarObjType::Ratchet) {
                            econVar(kVar).activeNow = true;
                        }
                    }
                }
                ReportEconomicVariable(state, "Ratchets", false, false, tariff(iTariff).tariffName);
                //---- Qualifies
                for (auto &e : econVar)
                    e.activeNow = false;
                for (kVar = 1; kVar <= state.dataEconTariff->numEconVar; ++kVar) {
                    if (econVar(kVar).tariffIndx == iTariff) {
                        if (econVar(kVar).kindOfObj == iEconVarObjType::Qualify) {
                            econVar(kVar).activeNow = true;
                        }
                    }
                }
                ReportEconomicVariable(state, "Qualifies", false, false, tariff(iTariff).tariffName);
                //---- Native Variables
                for (auto &e : econVar)
                    e.activeNow = false;
                for (kVar = tariff(iTariff).firstNative; kVar <= tariff(iTariff).lastNative; ++kVar) {
                    econVar(kVar).activeNow = true;
                }
                ReportEconomicVariable(state, "Native Variables", false, false, tariff(iTariff).tariffName);
                //---- Other Variables
                for (auto &e : econVar)
                    e.activeNow = false;
                for (kVar = 1; kVar <= state.dataEconTariff->numEconVar; ++kVar) {
                    if (econVar(kVar).tariffIndx == iTariff) {
                        if (!econVar(kVar).isReported) {
                            econVar(kVar).activeNow = true;
                        }
                    }
                }
                ReportEconomicVariable(state, "Other Variables", false, false, tariff(iTariff).tariffName);
                //---- Computation
                if (computation(iTariff).isUserDef) {
                    WriteTextLine(state, "Computation -  User Defined", true);
                } else {
                    WriteTextLine(state, "Computation -  Automatic", true);
                }
                outString = "";
                for (lStep = computation(iTariff).firstStep; lStep <= computation(iTariff).lastStep; ++lStep) {
                    curStep = state.dataEconTariff->steps(lStep);
                    {
                        auto const SELECT_CASE_var(curStep);
                        if (SELECT_CASE_var == 0) { // end of line
                            WriteTextLine(state, rstrip(outString));
                            outString = "";
                        } else if ((SELECT_CASE_var >= 1)) { // all positive values are a reference to an econVar
                            outString = econVar(curStep).name + ' ' + outString;
                        } else if (SELECT_CASE_var == opSUM) {
                            outString = "SUM " + outString;
                        } else if (SELECT_CASE_var == opMULTIPLY) {
                            outString = "MULTIPLY " + outString;
                        } else if (SELECT_CASE_var == opSUBTRACT) {
                            outString = "SUBTRACT " + outString;
                        } else if (SELECT_CASE_var == opDIVIDE) {
                            outString = "DIVIDE " + outString;
                        } else if (SELECT_CASE_var == opABSOLUTE) {
                            outString = "ABSOLUTE " + outString;
                        } else if (SELECT_CASE_var == opINTEGER) {
                            outString = "INTEGER " + outString;
                        } else if (SELECT_CASE_var == opSIGN) {
                            outString = "SIGN " + outString;
                        } else if (SELECT_CASE_var == opROUND) {
                            outString = "ROUND " + outString;
                        } else if (SELECT_CASE_var == opMAXIMUM) {
                            outString = "MAXIMUM " + outString;
                        } else if (SELECT_CASE_var == opMINIMUM) {
                            outString = "MINIMUM " + outString;
                        } else if (SELECT_CASE_var == opEXCEEDS) {
                            outString = "EXCEEDS " + outString;
                        } else if (SELECT_CASE_var == opANNUALMINIMUM) {
                            outString = "ANNUALMINIMUM " + outString;
                        } else if (SELECT_CASE_var == opANNUALMAXIMUM) {
                            outString = "ANNUALMAXIMUM " + outString;
                        } else if (SELECT_CASE_var == opANNUALSUM) {
                            outString = "ANNUALSUM " + outString;
                        } else if (SELECT_CASE_var == opANNUALAVERAGE) {
                            outString = "ANNUALAVERAGE " + outString;
                        } else if (SELECT_CASE_var == opANNUALOR) {
                            outString = "ANNUALOR " + outString;
                        } else if (SELECT_CASE_var == opANNUALAND) {
                            outString = "ANNUALAND " + outString;
                        } else if (SELECT_CASE_var == opANNUALMAXIMUMZERO) {
                            outString = "ANNUALMAXIMUMZERO " + outString;
                        } else if (SELECT_CASE_var == opANNUALMINIMUMZERO) {
                            outString = "ANNUALMINIMUMZERO " + outString;
                        } else if (SELECT_CASE_var == opIF) {
                            outString = "IF " + outString;
                        } else if (SELECT_CASE_var == opGREATERTHAN) {
                            outString = "GREATERTHAN " + outString;
                        } else if (SELECT_CASE_var == opGREATEREQUAL) {
                            outString = "GREATEREQUAL " + outString;
                        } else if (SELECT_CASE_var == opLESSTHAN) {
                            outString = "LESSTHAN " + outString;
                        } else if (SELECT_CASE_var == opLESSEQUAL) {
                            outString = "LESSEQUAL " + outString;
                        } else if (SELECT_CASE_var == opEQUAL) {
                            outString = "EQUAL " + outString;
                        } else if (SELECT_CASE_var == opNOTEQUAL) {
                            outString = "NOTEQUAL " + outString;
                        } else if (SELECT_CASE_var == opAND) {
                            outString = "AND " + outString;
                        } else if (SELECT_CASE_var == opOR) {
                            outString = "OR " + outString;
                        } else if (SELECT_CASE_var == opNOT) {
                            outString = "NOT " + outString;
                        } else if (SELECT_CASE_var == opADD) {
                            outString = "ADD " + outString;
                        } else if (SELECT_CASE_var == opNOOP) { // should clear the outString when done debugging
                            // outString = ''
                            outString = "FROM " + outString;
                        }
                    }
                }
            }
        }
    }
}

void showWarningsBasedOnTotal(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //    Get the annual maximum and sum for the econVariable.

    int iTariff;
    auto &tariff(state.dataEconTariff->tariff);

    if (state.dataEconTariff->numTariff > 0) {
        for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
            {
                auto const SELECT_CASE_var(tariff(iTariff).buyOrSell);
                if (SELECT_CASE_var == buyFromUtility) {
                    if (tariff(iTariff).totalAnnualCost < 0) {
                        ShowWarningError(state,
                                         "UtilityCost:Tariff: A negative annual total cost when buying electricity from a utility is unusual. ");
                        ShowContinueError(state, "  In UtilityCost:Tariff named " + tariff(iTariff).tariffName);
                    }
                } else if (SELECT_CASE_var == sellToUtility) {
                    if (tariff(iTariff).totalAnnualCost > 0) {
                        ShowWarningError(state,
                                         "UtilityCost:Tariff: A positive annual total cost when selling electricity to a utility is unusual. ");
                        ShowContinueError(state, "  In UtilityCost:Tariff named " + tariff(iTariff).tariffName);
                    }
                }
            }
        }
    }
}

void getMaxAndSum(EnergyPlusData &state, int const varPointer, Real64 &sumResult, Real64 &maxResult)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //    Get the annual maximum and sum for the econVariable.

    Real64 sumVal;
    Real64 maximumVal(0.0); // Autodesk Value not used but suppresses warning about HUGE_() call
    Real64 curVal;
    int jMonth;

    auto &econVar(state.dataEconTariff->econVar);

    sumVal = 0.0;
    maximumVal = -HUGE_(maximumVal);
    for (jMonth = 1; jMonth <= 12; ++jMonth) { // note not all months get printed out if more than 12 are used.- need to fix this later
        curVal = econVar(varPointer).values(jMonth);
        sumVal += curVal;
        if (curVal > maximumVal) {
            maximumVal = curVal;
        }
    }
    sumResult = sumVal;
    maxResult = maximumVal;
}

void ReportEconomicVariable(
    EnergyPlusData &state, std::string const &titleString, bool const includeCategory, bool const showCurrencySymbol, std::string const &forString)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004
    //    MODIFIED       January 2010, Kyle Benne
    //                   Added sqlite output

    //    Report all econVar that show as activeNow

    using OutputReportTabular::RealToStr;
    using OutputReportTabular::WriteReportHeaders;
    using OutputReportTabular::WriteSubtitle;
    using OutputReportTabular::WriteTable;

    // all arrays are in the format: (row, column)
    Array1D_string columnHead;
    Array1D_int columnWidth;
    Array1D_string rowHead;
    Array2D_string tableBody;
    Real64 sumVal;
    Real64 maximumVal;
    Real64 curVal;
    int curIndex;
    int curCatPt;
    int curCategory;

    int iVar;
    int jMonth;
    int cntOfVar;
    int nCntOfVar;

    auto &econVar(state.dataEconTariff->econVar);
    auto &chargeBlock(state.dataEconTariff->chargeBlock);
    auto &chargeSimple(state.dataEconTariff->chargeSimple);

    cntOfVar = 0;
    for (iVar = 1; iVar <= state.dataEconTariff->numEconVar; ++iVar) {
        if (econVar(iVar).activeNow) {
            ++cntOfVar;
        }
    }
    if (includeCategory) {
        rowHead.allocate(cntOfVar);
        columnHead.allocate(15);
        columnWidth.allocate(15);
        tableBody.allocate(15, cntOfVar);
    } else {
        rowHead.allocate(cntOfVar);
        columnHead.allocate(14);
        columnWidth.allocate(14);
        tableBody.allocate(14, cntOfVar);
    }
    // column names
    columnHead(1) = "Jan";
    columnHead(2) = "Feb";
    columnHead(3) = "Mar";
    columnHead(4) = "Apr";
    columnHead(5) = "May";
    columnHead(6) = "Jun";
    columnHead(7) = "Jul";
    columnHead(8) = "Aug";
    columnHead(9) = "Sep";
    columnHead(10) = "Oct";
    columnHead(11) = "Nov";
    columnHead(12) = "Dec";
    columnHead(13) = "Sum";
    columnHead(14) = "Max";
    if (includeCategory) {
        columnHead(15) = "Category";
    }
    nCntOfVar = 0;
    // row names
    for (iVar = 1; iVar <= state.dataEconTariff->numEconVar; ++iVar) {
        if (econVar(iVar).activeNow) {
            ++nCntOfVar;
            if (showCurrencySymbol) {
                rowHead(nCntOfVar) = econVar(iVar).name + " (~~$~~)";
            } else {
                rowHead(nCntOfVar) = econVar(iVar).name;
            }
        }
    }
    // fill the body
    nCntOfVar = 0;
    for (iVar = 1; iVar <= state.dataEconTariff->numEconVar; ++iVar) {
        if (econVar(iVar).activeNow) {
            ++nCntOfVar;
            for (jMonth = 1; jMonth <= 12; ++jMonth) { // note not all months get printed out if more than 12 are used.- need to fix this later
                curVal = econVar(iVar).values(jMonth);
                if ((curVal > 0) && (curVal < 1)) {
                    tableBody(jMonth, nCntOfVar) = RealToStr(curVal, 4);
                } else {
                    tableBody(jMonth, nCntOfVar) = RealToStr(curVal, 2);
                }
            }
            getMaxAndSum(state, iVar, sumVal, maximumVal);
            tableBody(13, nCntOfVar) = RealToStr(sumVal, 2);
            tableBody(14, nCntOfVar) = RealToStr(maximumVal, 2);
            if (includeCategory) {
                // first find category
                curCategory = 0;
                curIndex = econVar(iVar).index;

                switch (econVar(iVar).kindOfObj) {
                case iEconVarObjType::ChargeSimple:
                    if ((curIndex >= 1) && (curIndex <= state.dataEconTariff->numChargeSimple)) {
                        curCatPt = chargeSimple(curIndex).categoryPt;
                    }
                    break;
                case iEconVarObjType::ChargeBlock:
                    if ((curIndex >= 1) && (curIndex <= state.dataEconTariff->numChargeBlock)) {
                        curCatPt = chargeBlock(curIndex).categoryPt;
                    }
                    break;
                default:
                    break;
                }

                if ((curCatPt >= 1) && (curCatPt <= state.dataEconTariff->numEconVar)) {
                    curCategory = econVar(curCatPt).specific;
                }
                {
                    auto const SELECT_CASE_var(curCategory);
                    if (SELECT_CASE_var == catEnergyCharges) {
                        tableBody(15, nCntOfVar) = "EnergyCharges";
                    } else if (SELECT_CASE_var == catDemandCharges) {
                        tableBody(15, nCntOfVar) = "DemandCharges";
                    } else if (SELECT_CASE_var == catServiceCharges) {
                        tableBody(15, nCntOfVar) = "ServiceCharges";
                    } else if (SELECT_CASE_var == catBasis) {
                        tableBody(15, nCntOfVar) = "Basis";
                    } else if (SELECT_CASE_var == catAdjustment) {
                        tableBody(15, nCntOfVar) = "Adjustment";
                    } else if (SELECT_CASE_var == catSurcharge) {
                        tableBody(15, nCntOfVar) = "Surcharge";
                    } else if (SELECT_CASE_var == catSubtotal) {
                        tableBody(15, nCntOfVar) = "Subtotal";
                    } else if (SELECT_CASE_var == catTaxes) {
                        tableBody(15, nCntOfVar) = "Taxes";
                    } else if (SELECT_CASE_var == catTotal) {
                        tableBody(15, nCntOfVar) = "Total";
                    } else {
                        tableBody(15, nCntOfVar) = "none";
                    }
                }
            }
            econVar(iVar).isReported = true;
        }
    }
    columnWidth = 14; // array assignment - same for all columns
    WriteSubtitle(state, titleString);
    WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(tableBody, rowHead, columnHead, "Tariff Report", forString, titleString);
    }
    if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
        state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
            tableBody, rowHead, columnHead, "Tariff Report", forString, titleString);
    }
    columnHead.deallocate();
    rowHead.deallocate();
    columnWidth.deallocate();
    tableBody.deallocate();
}

void selectTariff(EnergyPlusData &state)
{
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2004

    //    To select tariffs for each combination of meter and
    //    group.  If multipler tariffs have the same meter and
    //    group, then select the one with the lowest cost.
    //    For electric tariffs, since they may have buy, sell, or
    //    netmetering, they need to be combined more carefully.
    //    Multiple meters are used but buy + sell might be more or
    //    less expensive than netmeter.

    int totalVarPt;
    int totEneVarPt;
    Real64 annualTotal;
    Real64 annEneTotal;
    int iTariff;
    int jMonth;
    int kTariff;
    int lMin;
    int mGroup;
    Array1D_int groupIndex;     // index number (in tariff) for the group name
    Array1D_int MinTariffIndex; // tariff index for the Minimum value
    int numMins;
    int curMinTariffIndex;
    bool isFound;
    int groupCount;
    int lowestSimpleTariff;
    int lowestPurchaseTariff;
    int lowestSurplusSoldTariff;
    int lowestNetMeterTariff;

    auto &tariff(state.dataEconTariff->tariff);
    auto &econVar(state.dataEconTariff->econVar);

    groupIndex.dimension(state.dataEconTariff->numTariff, 0);
    groupCount = 0;
    numMins = 0;
    MinTariffIndex.dimension(state.dataEconTariff->numTariff, 0);
    for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
        // compute the total annual cost of each tariff
        totalVarPt = tariff(iTariff).ptTotal;
        totEneVarPt = tariff(iTariff).nativeTotalEnergy;
        annualTotal = 0.0;
        annEneTotal = 0.0;
        for (jMonth = 1; jMonth <= MaxNumMonths; ++jMonth) {
            annualTotal += econVar(totalVarPt).values(jMonth);
            annEneTotal += econVar(totEneVarPt).values(jMonth);
        }
        tariff(iTariff).totalAnnualCost = annualTotal;
        tariff(iTariff).totalAnnualEnergy = annEneTotal;
        // Set the groupIndex
        if (groupIndex(iTariff) == 0) {
            // set the current item to the tariff index
            ++groupCount;
            groupIndex(iTariff) = groupCount;
            // set all remaining matching items to the same index
            for (kTariff = iTariff + 1; kTariff <= state.dataEconTariff->numTariff; ++kTariff) {
                if (UtilityRoutines::SameString(tariff(kTariff).groupName, tariff(iTariff).groupName)) {
                    groupIndex(kTariff) = groupCount;
                }
            }
        }
    }
    // First process the all tariff and identify the lowest cost for each type of meter and group.
    for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
        if (tariff(iTariff).isQualified) {
            isFound = false;
            for (lMin = 1; lMin <= numMins; ++lMin) {
                curMinTariffIndex = MinTariffIndex(lMin);
                // find matching meter and group
                if (tariff(iTariff).reportMeterIndx == tariff(curMinTariffIndex).reportMeterIndx) {
                    if (groupIndex(iTariff) == groupIndex(curMinTariffIndex)) {
                        isFound = true;
                        // found the matching mater and group now test if smaller Min is current tariff
                        if (tariff(iTariff).totalAnnualCost < tariff(curMinTariffIndex).totalAnnualCost) {
                            MinTariffIndex(lMin) = iTariff;
                            // select the new Minimum tariff and deselect the one that was just exceeded
                            tariff(curMinTariffIndex).isSelected = false;
                            tariff(iTariff).isSelected = true;
                        }
                    }
                }
            }
            if (!isFound) {
                ++numMins;
                if (numMins > state.dataEconTariff->numTariff) {
                    ShowWarningError(state, "UtilityCost:Tariff Debugging error numMins greater than numTariff.");
                }
                MinTariffIndex(numMins) = iTariff;
                // tariff(numMins)%isSelected = .TRUE.  !original
                tariff(iTariff).isSelected = true; // BTG changed 2/7/2005     CR6573
            }
        }
    }
    // Now select for the electric meters. If electric buying and selling and netmetering all are going
    // on, need to determine which combination should be selected. Within each group select just one set
    // of electric results.  The electric results can be either the buy rate only, the buy rate plus the
    // sell rate, or the netmetering rate, whichever of these three is the lowest combination.
    // (The kindElectricMtr was assigned in GetInputEconomicsTariff)
    for (mGroup = 1; mGroup <= groupCount; ++mGroup) {
        lowestSimpleTariff = 0;
        lowestPurchaseTariff = 0;
        lowestSurplusSoldTariff = 0;
        lowestNetMeterTariff = 0;
        for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
            if (tariff(iTariff).isQualified) {
                if (tariff(iTariff).isSelected) {
                    if (groupIndex(iTariff) == mGroup) {
                        {
                            auto const SELECT_CASE_var(tariff(iTariff).kindElectricMtr);
                            if (SELECT_CASE_var == kindMeterElecSimple) {
                                lowestSimpleTariff = iTariff;
                            } else if (SELECT_CASE_var == kindMeterElecProduced) {
                                // don't show electric produced rates as ever selected since surplus sold is more relevant
                                tariff(iTariff).isSelected = false;
                            } else if (SELECT_CASE_var == kindMeterElecPurchased) {
                                lowestPurchaseTariff = iTariff;
                            } else if (SELECT_CASE_var == kindMeterElecSurplusSold) {
                                lowestSurplusSoldTariff = iTariff;
                            } else if (SELECT_CASE_var == kindMeterElecNet) {
                                lowestNetMeterTariff = iTariff;
                            }
                        }
                    }
                }
            }
        }
        // compare the simple and purchased metered tariffs
        if ((lowestSimpleTariff > 0) && (lowestPurchaseTariff > 0)) {
            if (tariff(lowestSimpleTariff).totalAnnualCost < tariff(lowestPurchaseTariff).totalAnnualCost) {
                tariff(lowestPurchaseTariff).isSelected = false;
                lowestPurchaseTariff = 0;
            } else {
                tariff(lowestSimpleTariff).isSelected = false;
                lowestSimpleTariff = 0;
            }
        }
        // if surplus sold is negative use it otherwise don't
        if (lowestSurplusSoldTariff > 0) {
            if (tariff(lowestSurplusSoldTariff).totalAnnualCost > 0) {
                tariff(lowestSurplusSoldTariff).isSelected = false;
                lowestSurplusSoldTariff = 0;
            }
        }
        // if netmetering is used compare it to simple plus surplus
        if (((lowestNetMeterTariff > 0) && (lowestSurplusSoldTariff > 0)) && (lowestSimpleTariff > 0)) {
            if (tariff(lowestNetMeterTariff).totalAnnualCost <
                (tariff(lowestSimpleTariff).totalAnnualCost + tariff(lowestSurplusSoldTariff).totalAnnualCost)) {
                tariff(lowestSimpleTariff).isSelected = false;
                lowestSimpleTariff = 0;
                tariff(lowestSurplusSoldTariff).isSelected = false;
                lowestSurplusSoldTariff = 0;
            } else {
                tariff(lowestNetMeterTariff).isSelected = false;
                lowestNetMeterTariff = 0;
            }
        }
        // if netmetering is used compare it to purchased plus surplus
        if (((lowestNetMeterTariff > 0) && (lowestSurplusSoldTariff > 0)) && (lowestPurchaseTariff > 0)) {
            if (tariff(lowestNetMeterTariff).totalAnnualCost <
                (tariff(lowestPurchaseTariff).totalAnnualCost + tariff(lowestSurplusSoldTariff).totalAnnualCost)) {
                tariff(lowestPurchaseTariff).isSelected = false;
                lowestPurchaseTariff = 0;
                tariff(lowestSurplusSoldTariff).isSelected = false;
                lowestSurplusSoldTariff = 0;
            } else {
                tariff(lowestNetMeterTariff).isSelected = false;
                lowestNetMeterTariff = 0;
            }
        }
        // if netmetering is used compare it to simple only
        if ((lowestNetMeterTariff > 0) && (lowestSimpleTariff > 0)) {
            if (tariff(lowestNetMeterTariff).totalAnnualCost < tariff(lowestSimpleTariff).totalAnnualCost) {
                tariff(lowestSimpleTariff).isSelected = false;
                lowestSimpleTariff = 0;
            } else {
                tariff(lowestNetMeterTariff).isSelected = false;
                lowestNetMeterTariff = 0;
            }
        }
        // if netmetering is used compare it to purchased only
        if ((lowestNetMeterTariff > 0) && (lowestPurchaseTariff > 0)) {
            if (tariff(lowestNetMeterTariff).totalAnnualCost < tariff(lowestPurchaseTariff).totalAnnualCost) {
                tariff(lowestPurchaseTariff).isSelected = false;
                lowestPurchaseTariff = 0;
            } else {
                tariff(lowestNetMeterTariff).isSelected = false;
                lowestNetMeterTariff = 0;
            }
        }
    }
    groupIndex.deallocate();
    MinTariffIndex.deallocate();
}

void GetMonthlyCostForResource(EnergyPlusData &state, DataGlobalConstants::ResourceType const inResourceNumber, Array1A<Real64> outMonthlyCosts)
{
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   May 2010

    //  Return the total annual cost for a given resource number.

    // Argument array dimensioning
    outMonthlyCosts.dim(12);

    int iTariff;
    int jMonth;
    int totalVarPt;

    auto &tariff(state.dataEconTariff->tariff);
    auto &econVar(state.dataEconTariff->econVar);

    outMonthlyCosts = 0.0;
    for (iTariff = 1; iTariff <= state.dataEconTariff->numTariff; ++iTariff) {
        if (tariff(iTariff).isSelected) {
            if (tariff(iTariff).resourceNum == inResourceNumber) {
                totalVarPt = tariff(iTariff).ptTotal;
                for (jMonth = 1; jMonth <= 12; ++jMonth) { // use 12 because LCC assume 12 months
                    outMonthlyCosts(jMonth) += econVar(totalVarPt).values(jMonth);
                }
            }
        }
    }
}

} // namespace EnergyPlus::EconomicTariff
