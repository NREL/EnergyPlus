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

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/CostEstimateManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EconomicLifeCycleCost.hh>
#include <EnergyPlus/EconomicTariff.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/ResultsFramework.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::EconomicLifeCycleCost {

// Module containing the routines dealing with the EconomicLifeCycleCost

// MODULE INFORMATION:
//       AUTHOR         Jason Glazer of GARD Analytics, Inc.
//       DATE WRITTEN   May 2010
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
//   To compute life-cycle cost measures such as present value based
//   on input provided by the user as well as calculated energy costs.

// METHODOLOGY EMPLOYED:
//   Uses NIST Handbook 135 "Life-Cycle Costing Manual for the Federal
//   Energy Management Program" for most computations.

// REFERENCES:
//   To compute the net present value for all costs entered in the
//   LifeCycleCosts objects, the algorithms from NIST Handbook 135
//   "Life-Cycle Costing Manual for the Federal Energy Management
//   Program" will be used as the primary source. Supplemental sources
//   of algorithms will be derived from ASTM E833-09a "Standard
//   Terminology of Building Economics", ASTM E917-05 "Standard
//   Practice for Measuring Life-Cycle Cost of Buildings and Building
//   Systems", and "Engineering Economic Analysis, Ninth Edition", by
//   Donald Newnan, Ted Eschenback, and Jerome Lavelle.

// OTHER NOTES:
// na

// Using/Aliasing
using namespace DataGlobalConstants;

// Functions

void GetInputForLifeCycleCost(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2010
    //    MODIFIED       na
    //    RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //    Read the input file for "LifeCycleCost:Parameters" object.

    // Using/Aliasing
    using OutputReportTabular::AddTOCEntry;

    auto &elcc(state.dataEconLifeCycleCost);

    if (elcc->GetInput_GetLifeCycleCostInput) {
        GetInputLifeCycleCostParameters(state);
        GetInputLifeCycleCostRecurringCosts(state);
        GetInputLifeCycleCostNonrecurringCost(state);
        GetInputLifeCycleCostUsePriceEscalation(state);
        GetInputLifeCycleCostUseAdjustment(state);
        elcc->GetInput_GetLifeCycleCostInput = false;
    }
}

void ComputeLifeCycleCostAndReport(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2010
    //    MODIFIED       na
    //    RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //    Perform the life cycle cost computations and write report.

    if (state.dataEconLifeCycleCost->LCCparamPresent) {
        DisplayString(state, "Computing Life Cycle Costs and Reporting");
        ExpressAsCashFlows(state);
        ComputePresentValue(state);
        ComputeEscalatedEnergyCosts(state);
        ComputeTaxAndDepreciation(state);
        WriteTabularLifeCycleCostReport(state);
    }
}

//======================================================================================================================
//======================================================================================================================

//    GET INPUT ROUTINES

//======================================================================================================================
//======================================================================================================================

void GetInputLifeCycleCostParameters(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2010

    // PURPOSE OF THIS SUBROUTINE:
    //    Read the input file for "LifeCycleCost:Parameters" object.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int jFld;                        // loop counter
    int NumFields;                   // Total number of elements
    int NumAlphas;                   // Number of elements in the alpha array
    int NumNums;                     // Number of elements in the numeric array
    Array1D_string AlphaArray;       // character string data
    Array1D<Real64> NumArray;        // numeric data
    int IOStat;                      // IO Status when calling get input subroutine
    std::string CurrentModuleObject; // for ease in renaming.
    int NumObj;                      // count of objects

    CurrentModuleObject = "LifeCycleCost:Parameters";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
    NumArray.allocate(NumNums);
    AlphaArray.allocate(NumAlphas);
    NumObj = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

    auto &elcc(state.dataEconLifeCycleCost);

    if (NumObj == 0) {
        elcc->LCCparamPresent = false;
    } else if (NumObj == 1) {
        elcc->LCCparamPresent = true;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 1,
                                                                 AlphaArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another life cycle cost object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                ShowWarningError(state,
                                 "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                     " a field was found containing LifeCycleCost: which may indicate a missing comma.");
            }
        }
        // start to extract values from input array into appropriate fields
        //  A1,  \field Name
        //       \required-field
        //       \type alpha
        elcc->LCCname = AlphaArray(1);
        //  A2, \field Discounting Convention
        //      \type choice
        //      \key EndOfYear
        //      \key MidYear
        //      \key BeginningOfYear
        //      \default EndOfYear
        elcc->discountConvention = static_cast<DiscConv>(getEnumerationValue(DiscConvNamesUC, UtilityRoutines::MakeUPPERCase(AlphaArray(2))));
        if (elcc->discountConvention == DiscConv::Unassigned) {
            elcc->discountConvention = DiscConv::EndOfYear;
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + AlphaArray(2) +
                                 "\". EndOfYear will be used.");
        }
        // A3,  \field Inflation Approach
        //      \type choice
        //      \key ConstantDollar
        //      \key CurrentDollar
        //      \default ConstantDollar
        elcc->inflationApproach = static_cast<InflAppr>(getEnumerationValue(InflApprNamesUC, UtilityRoutines::MakeUPPERCase(AlphaArray(3))));
        if (elcc->inflationApproach == InflAppr::Unassigned) {
            elcc->inflationApproach = InflAppr::ConstantDollar;
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + AlphaArray(3) +
                                 "\". ConstantDollar will be used.");
        }
        // N1,  \field Real Discount Rate
        //      \type real
        elcc->realDiscountRate = NumArray(1);
        if ((elcc->inflationApproach == InflAppr::ConstantDollar) && state.dataIPShortCut->lNumericFieldBlanks(1)) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid for field " + state.dataIPShortCut->cNumericFieldNames(1) +
                                 " to be blank when ConstantDollar analysis is be used.");
        }
        if ((elcc->realDiscountRate > 0.30) || (elcc->realDiscountRate < -0.30)) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(1) +
                                 ".  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. ");
        }
        // N2,  \field Nominal Discount Rate
        //      \type real
        elcc->nominalDiscountRate = NumArray(2);
        if ((elcc->inflationApproach == InflAppr::CurrentDollar) && state.dataIPShortCut->lNumericFieldBlanks(2)) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid for field " + state.dataIPShortCut->cNumericFieldNames(2) +
                                 " to be blank when CurrentDollar analysis is be used.");
        }
        if ((elcc->nominalDiscountRate > 0.30) || (elcc->nominalDiscountRate < -0.30)) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(2) +
                                 ".  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. ");
        }
        // N3,  \field Inflation
        //      \type real
        elcc->inflation = NumArray(3);
        if ((elcc->inflationApproach == InflAppr::ConstantDollar) && (!state.dataIPShortCut->lNumericFieldBlanks(3))) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid for field " + state.dataIPShortCut->cNumericFieldNames(3) +
                                 " contain a value when ConstantDollar analysis is be used.");
        }
        if ((elcc->inflation > 0.30) || (elcc->inflation < -0.30)) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(3) +
                                 ".  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. ");
        }
        // A4,  \field Base Date Month
        //      \type choice
        //      \key January
        //      \key February
        //      \key March
        //      \key April
        //      \key May
        //      \key June
        //      \key July
        //      \key August
        //      \key September
        //      \key October
        //      \key November
        //      \key December
        //      \default January
        elcc->baseDateMonth = getEnumerationValue(UtilityRoutines::MonthNamesUC, UtilityRoutines::MakeUPPERCase(AlphaArray(4)));
        if (elcc->baseDateMonth == -1) {
            elcc->baseDateMonth = 0;
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid month entered in field " + state.dataIPShortCut->cAlphaFieldNames(4) +
                                 ". Using January instead of \"" + AlphaArray(4) + "\"");
        }
        // N4,  \field Base Date Year
        //      \type integer
        //      \minimum 1900
        //      \maximum 2100
        elcc->baseDateYear = int(NumArray(4));
        if (elcc->baseDateYear > 2100) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(4) +
                                 ".  Value greater than 2100 yet it is representing a year. ");
        }
        if (elcc->baseDateYear < 1900) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(4) +
                                 ".  Value less than 1900 yet it is representing a year. ");
        }
        // A5,  \field Service Date Month
        //      \type choice
        //      \key January
        //      \key February
        //      \key March
        //      \key April
        //      \key May
        //      \key June
        //      \key July
        //      \key August
        //      \key September
        //      \key October
        //      \key November
        //      \key December
        //      \default January
        elcc->serviceDateMonth = getEnumerationValue(UtilityRoutines::MonthNamesUC, UtilityRoutines::MakeUPPERCase(AlphaArray(5)));
        if (elcc->serviceDateMonth == -1) {
            elcc->serviceDateMonth = 0;
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid month entered in field " + state.dataIPShortCut->cAlphaFieldNames(5) +
                                 ". Using January instead of \"" + AlphaArray(5) + "\"");
        }
        // N5,  \field Service Date Year
        //      \type integer
        //      \minimum 1900
        //      \maximum 2100
        elcc->serviceDateYear = int(NumArray(5));
        if (elcc->serviceDateYear > 2100) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(5) +
                                 ".  Value greater than 2100 yet it is representing a year. ");
        }
        if (elcc->serviceDateYear < 1900) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(5) +
                                 ".  Value less than 1900 yet it is representing a year. ");
        }
        // N6,  \field Length of Study Period in Years
        //      \type integer
        //      \minimum 1
        //      \maximum 100
        elcc->lengthStudyYears = int(NumArray(6));
        if (elcc->lengthStudyYears > 100) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(6) +
                                 ".  A value greater than 100 is not reasonable for an economic evaluation. ");
        }
        if (elcc->lengthStudyYears < 1) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(6) +
                                 ".  A value less than 1 is not reasonable for an economic evaluation. ");
        }
        elcc->lengthStudyTotalMonths = elcc->lengthStudyYears * 12;
        // N7, \field Tax rate
        //      \type real
        //      \minimum 0.0
        elcc->taxRate = NumArray(7);
        if (elcc->taxRate < 0.0 && (!state.dataIPShortCut->lNumericFieldBlanks(7))) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(10) +
                                 ".  A value less than 0 is not reasonable for a tax rate. ");
        }
        // A6;  \field Depreciation Method
        //      \type choice
        //      \key ModifiedAcceleratedCostRecoverySystem-3year
        //      \key ModifiedAcceleratedCostRecoverySystem-5year
        //      \key ModifiedAcceleratedCostRecoverySystem-7year
        //      \key ModifiedAcceleratedCostRecoverySystem-10year
        //      \key ModifiedAcceleratedCostRecoverySystem-15year
        //      \key ModifiedAcceleratedCostRecoverySystem-20year
        //      \key StraightLine-27year
        //      \key StraightLine-31year
        //      \key StraightLine-39year
        //      \key StraightLine-40year
        //      \key None
        //      \default None
        elcc->depreciationMethod = static_cast<DeprMethod>(getEnumerationValue(DeprMethodNamesUC, UtilityRoutines::MakeUPPERCase(AlphaArray(6))));
        if (elcc->depreciationMethod == DeprMethod::Unassigned) {
            elcc->depreciationMethod = DeprMethod::None;
            if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                ShowWarningError(state,
                                 CurrentModuleObject + ": The input field " + state.dataIPShortCut->cAlphaFieldNames(6) +
                                     "is blank. \"None\" will be used.");
            } else {
                ShowWarningError(state,
                                 CurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" + AlphaArray(6) +
                                     R"(". "None" will be used.)");
            }
        }
        // compute derived variables
        elcc->lastDateYear = elcc->baseDateYear + elcc->lengthStudyYears - 1;
    } else {
        ShowWarningError(state, CurrentModuleObject + ": Only one instance of this object is allowed. No life-cycle cost reports will be generated.");
        elcc->LCCparamPresent = false;
    }
}

void GetInputLifeCycleCostRecurringCosts(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2010

    // PURPOSE OF THIS SUBROUTINE:
    //    Read the input file for "LifeCycleCost:RecurringCosts" object.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int iInObj;                      // loop index variable for reading in objects
    int jFld;                        // loop counter
    int NumFields;                   // Total number of elements
    int NumAlphas;                   // Number of elements in the alpha array
    int NumNums;                     // Number of elements in the numeric array
    Array1D_string AlphaArray;       // character string data
    Array1D<Real64> NumArray;        // numeric data
    int IOStat;                      // IO Status when calling get input subroutine
    std::string CurrentModuleObject; // for ease in renaming.

    auto &elcc(state.dataEconLifeCycleCost);

    if (!elcc->LCCparamPresent) return;
    CurrentModuleObject = "LifeCycleCost:RecurringCosts";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
    NumArray.allocate(NumNums);
    AlphaArray.allocate(NumAlphas);
    elcc->numRecurringCosts = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    elcc->RecurringCosts.resize(elcc->numRecurringCosts);
    for (iInObj = 0; iInObj < elcc->numRecurringCosts; ++iInObj) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 iInObj + 1, // since this index needs to start from 1
                                                                 AlphaArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another life cycle cost object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                ShowWarningError(state,
                                 "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                     " a field was found containing LifeCycleCost: which may indicate a missing comma.");
            }
        }
        // start to extract values from input array into appropriate fields
        //   A1,  \field Name
        //        \required-field
        //        \type alpha
        elcc->RecurringCosts[iInObj].name = AlphaArray(1);
        //   A2,  \field Category
        //        \type choice
        //        \key Maintenance
        //        \key Repair
        //        \key Operation
        //        \key Replacement
        //        \key MinorOverhaul
        //        \key MajorOverhaul
        //        \key OtherOperational
        //        \default Maintenance
        elcc->RecurringCosts[iInObj].category =
            static_cast<CostCategory>(getEnumerationValue(CostCategoryNamesUCNoSpace, UtilityRoutines::MakeUPPERCase(AlphaArray(2))));
        bool isNotRecurringCost = BITF_TEST_NONE(BITF(elcc->RecurringCosts[iInObj].category),
                                                 BITF(CostCategory::Maintenance) | BITF(CostCategory::Repair) | BITF(CostCategory::Operation) |
                                                     BITF(CostCategory::Replacement) | BITF(CostCategory::MinorOverhaul) |
                                                     BITF(CostCategory::MajorOverhaul) | BITF(CostCategory::OtherOperational));
        if (isNotRecurringCost) {
            elcc->RecurringCosts[iInObj].category = CostCategory::Maintenance;
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + AlphaArray(2) +
                                 "\". The category of Maintenance will be used.");
        }
        //   N1,  \field Cost
        //        \type real
        elcc->RecurringCosts[iInObj].cost = NumArray(1);
        //   A3,  \field Start of Costs
        //        \type choice
        //        \key ServicePeriod
        //        \key BasePeriod
        //        \default ServicePeriod
        elcc->RecurringCosts[iInObj].startOfCosts =
            static_cast<StartCosts>(getEnumerationValue(StartCostNamesUC, UtilityRoutines::MakeUPPERCase(AlphaArray(3))));
        if (elcc->RecurringCosts[iInObj].startOfCosts == StartCosts::Unassigned) {
            elcc->RecurringCosts[iInObj].startOfCosts = StartCosts::ServicePeriod;
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + AlphaArray(3) +
                                 "\". The start of the service period will be used.");
        }
        //   N2,  \field Years from Start
        //        \type integer
        //        \minimum 0
        //        \maximum 100
        elcc->RecurringCosts[iInObj].yearsFromStart = int(NumArray(2));
        if (elcc->RecurringCosts[iInObj].yearsFromStart > 100) {
            ShowWarningError(
                state,
                CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(2) +
                    ".  This value is the number of years from the start so a value greater than 100 is not reasonable for an economic evaluation. ");
        }
        if (elcc->RecurringCosts[iInObj].yearsFromStart < 0) {
            ShowWarningError(
                state,
                CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(2) +
                    ".  This value is the number of years from the start so a value less than 0 is not reasonable for an economic evaluation. ");
        }
        //   N3,  \field Months from Start
        //        \type integer
        //        \minimum 0
        //        \maximum 1200
        elcc->RecurringCosts[iInObj].monthsFromStart = int(NumArray(3));
        if (elcc->RecurringCosts[iInObj].monthsFromStart > 1200) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(3) +
                                 ".  This value is the number of months from the start so a value greater than 1200 is not reasonable for an "
                                 "economic evaluation. ");
        }
        if (elcc->RecurringCosts[iInObj].monthsFromStart < 0) {
            ShowWarningError(
                state,
                CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(3) +
                    ".  This value is the number of months from the start so a value less than 0 is not reasonable for an economic evaluation. ");
        }
        //   N4,  \field Repeat Period Years
        //        \type integer
        //        \minimum 1
        //        \maximum 100
        elcc->RecurringCosts[iInObj].repeatPeriodYears = int(NumArray(4));
        if (elcc->RecurringCosts[iInObj].repeatPeriodYears > 100) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(4) +
                                 ".  This value is the number of years between occurrences of the cost so a value greater than 100 is not reasonable "
                                 "for an economic evaluation. ");
        }
        if (elcc->RecurringCosts[iInObj].repeatPeriodYears < 1) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(4) +
                                 ".  This value is the number of years between occurrences of the cost so a value less than 1 is not reasonable for "
                                 "an economic evaluation. ");
        }
        //   N5,  \field Repeat Period Months
        //        \type integer
        //        \minimum 0
        //        \maximum 1200
        elcc->RecurringCosts[iInObj].repeatPeriodMonths = int(NumArray(5));
        if (elcc->RecurringCosts[iInObj].repeatPeriodMonths > 1200) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(5) +
                                 ".  This value is the number of months between occurrences of the cost so a value greater than 1200 is not "
                                 "reasonable for an economic evaluation. ");
        }
        if (elcc->RecurringCosts[iInObj].repeatPeriodMonths < 0) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(5) +
                                 ".  This value is the number of months between occurrences of the cost so a value less than 0 is not reasonable for "
                                 "an economic evaluation. ");
        }
        if ((elcc->RecurringCosts[iInObj].repeatPeriodMonths == 0) && (elcc->RecurringCosts[iInObj].repeatPeriodYears == 0)) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in fields " + state.dataIPShortCut->cNumericFieldNames(5) + " and " +
                                 state.dataIPShortCut->cNumericFieldNames(4) + ".  The repeat period must not be zero months and zero years. ");
        }
        //   N6;  \field Annual escalation rate
        //        \type real
        elcc->RecurringCosts[iInObj].annualEscalationRate = int(NumArray(6));
        if (elcc->RecurringCosts[iInObj].annualEscalationRate > 0.30) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(6) +
                                 ".  This value is the decimal value for the annual escalation so most values are between 0.02 and 0.15. ");
        }
        if (elcc->RecurringCosts[iInObj].annualEscalationRate < -0.30) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(6) +
                                 ".  This value is the decimal value for the annual escalation so most values are between 0.02 and 0.15. ");
        }
        // express the years and months fields in total months
        elcc->RecurringCosts[iInObj].totalMonthsFromStart =
            elcc->RecurringCosts[iInObj].yearsFromStart * 12 + elcc->RecurringCosts[iInObj].monthsFromStart;
        elcc->RecurringCosts[iInObj].totalRepeatPeriodMonths =
            elcc->RecurringCosts[iInObj].repeatPeriodYears * 12 + elcc->RecurringCosts[iInObj].repeatPeriodMonths;
    }
}

void GetInputLifeCycleCostNonrecurringCost(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2010

    // PURPOSE OF THIS SUBROUTINE:
    //    Read the input file for "LifeCycleCost:NonrecurringCost" object.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int iInObj;                      // loop index variable for reading in objects
    int jFld;                        // loop counter
    int NumFields;                   // Total number of elements
    int NumAlphas;                   // Number of elements in the alpha array
    int NumNums;                     // Number of elements in the numeric array
    Array1D_string AlphaArray;       // character string data
    Array1D<Real64> NumArray;        // numeric data
    int IOStat;                      // IO Status when calling get input subroutine
    std::string CurrentModuleObject; // for ease in renaming.
    int numComponentCostLineItems;   // number of ComponentCost:LineItem objects

    auto &elcc(state.dataEconLifeCycleCost);

    if (!elcc->LCCparamPresent) return;
    CurrentModuleObject = "LifeCycleCost:NonrecurringCost";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
    NumArray.allocate(NumNums);
    AlphaArray.allocate(NumAlphas);
    elcc->numNonrecurringCost = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    numComponentCostLineItems = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ComponentCost:LineItem");
    if (numComponentCostLineItems > 0) {                              // leave room for component cost total
        elcc->NonrecurringCost.resize(elcc->numNonrecurringCost + 1); // add a place for CostEstimate total
    } else {
        elcc->NonrecurringCost.resize(elcc->numNonrecurringCost);
    }
    for (iInObj = 0; iInObj < elcc->numNonrecurringCost; ++iInObj) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 iInObj + 1, // since this index needs to start from 1
                                                                 AlphaArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // check to make sure none of the values are another life cycle cost object
        for (jFld = 1; jFld <= NumAlphas; ++jFld) {
            if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                ShowWarningError(state,
                                 "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                     " a field was found containing LifeCycleCost: which may indicate a missing comma.");
            }
        }
        // start to extract values from input array into appropriate fields
        // A1,  \field Name
        //      \required-field
        //      \type alpha
        elcc->NonrecurringCost[iInObj].name = AlphaArray(1);
        // A2,  \field Category
        //      \type choice
        //      \key Construction
        //      \key Salvage
        //      \key OtherCapital
        //      \default Construction
        elcc->NonrecurringCost[iInObj].category =
            static_cast<CostCategory>(getEnumerationValue(CostCategoryNamesUCNoSpace, UtilityRoutines::MakeUPPERCase(AlphaArray(2))));
        bool isNotNonRecurringCost =
            BITF_TEST_NONE(BITF(elcc->NonrecurringCost[iInObj].category),
                           BITF(CostCategory::Construction) | BITF(CostCategory::Salvage) | BITF(CostCategory::OtherCapital));
        if (isNotNonRecurringCost) {
            elcc->NonrecurringCost[iInObj].category = CostCategory::Construction;
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + AlphaArray(2) +
                                 "\". The category of Construction will be used.");
        }
        // N1,  \field Cost
        //      \type real
        elcc->NonrecurringCost[iInObj].cost = NumArray(1);
        // A3,  \field Start of Costs
        //      \type choice
        //      \key ServicePeriod
        //      \key BasePeriod
        //      \default ServicePeriod
        elcc->NonrecurringCost[iInObj].startOfCosts =
            static_cast<StartCosts>(getEnumerationValue(StartCostNamesUC, UtilityRoutines::MakeUPPERCase(AlphaArray(3))));
        if (elcc->NonrecurringCost[iInObj].startOfCosts == StartCosts::Unassigned) {
            elcc->NonrecurringCost[iInObj].startOfCosts = StartCosts::ServicePeriod;
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + AlphaArray(3) +
                                 "\". The start of the service period will be used.");
        }
        // N2,  \field Years from Start
        //      \type integer
        //      \minimum 0
        //      \maximum 100
        elcc->NonrecurringCost[iInObj].yearsFromStart = int(NumArray(2));
        if (elcc->NonrecurringCost[iInObj].yearsFromStart > 100) {
            ShowWarningError(
                state,
                CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(2) +
                    ".  This value is the number of years from the start so a value greater than 100 is not reasonable for an economic evaluation. ");
        }
        if (elcc->NonrecurringCost[iInObj].yearsFromStart < 0) {
            ShowWarningError(
                state,
                CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(2) +
                    ".  This value is the number of years from the start so a value less than 0 is not reasonable for an economic evaluation. ");
        }
        //  N3;  \field Months from Start
        //       \type integer
        //       \minimum 0
        //       \maximum 11
        elcc->NonrecurringCost[iInObj].monthsFromStart = int(NumArray(3));
        if (elcc->NonrecurringCost[iInObj].monthsFromStart > 1200) {
            ShowWarningError(state,
                             CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(3) +
                                 ".  This value is the number of months from the start so a value greater than 1200 is not reasonable for an "
                                 "economic evaluation. ");
        }
        if (elcc->NonrecurringCost[iInObj].monthsFromStart < 0) {
            ShowWarningError(
                state,
                CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(3) +
                    ".  This value is the number of months from the start so a value less than 0 is not reasonable for an economic evaluation. ");
        }
        // express the years and months fields in total months
        elcc->NonrecurringCost[iInObj].totalMonthsFromStart =
            elcc->NonrecurringCost[iInObj].yearsFromStart * 12 + elcc->NonrecurringCost[iInObj].monthsFromStart;
    }
}

void GetInputLifeCycleCostUsePriceEscalation(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2010

    // PURPOSE OF THIS SUBROUTINE:
    //    Read the input file for "LifeCycleCost:UsePriceEscalation" object.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int iInObj;                      // loop index variable for reading in objects
    int jFld;                        // loop counter
    int jYear;                       // loop counter
    int NumFields;                   // Total number of elements
    int NumAlphas;                   // Number of elements in the alpha array
    int NumNums;                     // Number of elements in the numeric array
    Array1D_string AlphaArray;       // character string data
    Array1D<Real64> NumArray;        // numeric data
    int IOStat;                      // IO Status when calling get input subroutine
    std::string CurrentModuleObject; // for ease in renaming.

    auto &elcc(state.dataEconLifeCycleCost);

    if (!elcc->LCCparamPresent) return;
    CurrentModuleObject = "LifeCycleCost:UsePriceEscalation";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
    NumArray.allocate(NumNums);
    AlphaArray.allocate(NumAlphas);
    elcc->numUsePriceEscalation = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    elcc->UsePriceEscalation.allocate(elcc->numUsePriceEscalation);
    for (iInObj = 1; iInObj <= elcc->numUsePriceEscalation; ++iInObj) {
        elcc->UsePriceEscalation(iInObj).Escalation.allocate(elcc->lengthStudyYears);
    }
    if (elcc->numUsePriceEscalation > 0) {
        for (iInObj = 1; iInObj <= elcc->numUsePriceEscalation; ++iInObj) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     iInObj,
                                                                     AlphaArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            // check to make sure none of the values are another life cycle cost object
            for (jFld = 1; jFld <= NumAlphas; ++jFld) {
                if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                    ShowWarningError(state,
                                     "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                         " a field was found containing LifeCycleCost: which may indicate a missing comma.");
                }
            }
            // start to extract values from input array into appropriate fields
            // A1,  \field Name
            //      \required-field
            //      \type alpha
            elcc->UsePriceEscalation(iInObj).name = AlphaArray(1);
            //  A2,  \field Resource
            //       \required-field
            //       \type choice
            //       \key Electricity
            //       \key NaturalGas
            //       \key Steam
            //       \key Gasoline
            //       \key Diesel
            //       \key Coal
            //       \key FuelOilNo1
            //       \key FuelOilNo2
            //       \key Propane
            //       \key Water
            //       \key OtherFuel1
            //       \key OtherFuel2
            elcc->UsePriceEscalation(iInObj).resource = AssignResourceTypeNum(AlphaArray(2)); // use function from DataGlobalConstants
            if (NumAlphas > 3) {
                ShowWarningError(state, "In " + CurrentModuleObject + " contains more alpha fields than expected.");
            }
            // N1,  \field Escalation Start Year
            //      \type integer
            //      \minimum 1900
            //      \maximum 2100
            elcc->UsePriceEscalation(iInObj).escalationStartYear = int(NumArray(1));
            if (elcc->UsePriceEscalation(iInObj).escalationStartYear > 2100) {
                ShowWarningError(state,
                                 CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(1) +
                                     ".  Value greater than 2100 yet it is representing a year. ");
            }
            if (elcc->UsePriceEscalation(iInObj).escalationStartYear < 1900) {
                ShowWarningError(state,
                                 CurrentModuleObject + ": Invalid value in field " + state.dataIPShortCut->cNumericFieldNames(1) +
                                     ".  Value less than 1900 yet it is representing a year. ");
            }
            // A3,  \field Escalation Start Month
            //      \type choice
            //      \key January
            //      \key February
            //      \key March
            //      \key April
            //      \key May
            //      \key June
            //      \key July
            //      \key August
            //      \key September
            //      \key October
            //      \key November
            //      \key December
            //      \default January
            elcc->UsePriceEscalation(iInObj).escalationStartMonth =
                getEnumerationValue(UtilityRoutines::MonthNamesUC, UtilityRoutines::MakeUPPERCase(AlphaArray(3)));
            if (elcc->UsePriceEscalation(iInObj).escalationStartMonth == -1) {
                elcc->UsePriceEscalation(iInObj).escalationStartMonth = 0;
                ShowWarningError(state,
                                 CurrentModuleObject + ": Invalid month entered in field " + state.dataIPShortCut->cAlphaFieldNames(3) +
                                     ". Using January instead of \"" + AlphaArray(3) + "\"");
            }
            // N2,  \field Year 1 Escalation
            //      \type real
            //      \begin-extensible
            // The array is from the baseDateYear until baseDateYear + lengthStudyYears
            // Set the array to default to 1.0
            for (jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
                elcc->UsePriceEscalation(iInObj).Escalation(jYear) = 1.0;
            }
            // Since the years in the UsePriceEscalation may not match up with the baseDateYear and
            // the lenghtStudyYears, need to make adjustments when reading in the values to align
            // with the baseDateYear (the first item in all yearly arrays)
            elcc->UsePriceEscalation_escStartYear = elcc->UsePriceEscalation(iInObj).escalationStartYear;
            elcc->UsePriceEscalation_escNumYears = NumNums - 1;
            elcc->UsePriceEscalation_escEndYear = elcc->UsePriceEscalation_escStartYear + elcc->UsePriceEscalation_escNumYears - 1;
            elcc->UsePriceEscalation_earlierEndYear = min(elcc->UsePriceEscalation_escEndYear, elcc->lastDateYear);   // pick the earlier ending date
            elcc->UsePriceEscalation_laterStartYear = max(elcc->UsePriceEscalation_escStartYear, elcc->baseDateYear); // pick the later starting date
            for (jYear = elcc->UsePriceEscalation_laterStartYear; jYear <= elcc->UsePriceEscalation_earlierEndYear; ++jYear) {
                elcc->UsePriceEscalation_curFld = 2 + jYear - elcc->UsePriceEscalation_escStartYear;
                elcc->UsePriceEscalation_curEsc = 1 + jYear - elcc->baseDateYear;
                if ((elcc->UsePriceEscalation_curFld <= NumNums) && (elcc->UsePriceEscalation_curFld >= 1)) {
                    if ((elcc->UsePriceEscalation_curEsc <= elcc->lengthStudyYears) && (elcc->UsePriceEscalation_curEsc >= 1)) {
                        elcc->UsePriceEscalation(iInObj).Escalation(elcc->UsePriceEscalation_curEsc) = NumArray(elcc->UsePriceEscalation_curFld);
                    }
                }
            }
        }
    }
}

void GetInputLifeCycleCostUseAdjustment(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   May 2010

    // PURPOSE OF THIS SUBROUTINE:
    //    Read the input file for "LifeCycleCost:UseAdjustment" object.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int iInObj;                      // loop index variable for reading in objects
    int jFld;                        // loop counter
    int jYear;                       // loop counter
    int NumFields;                   // Total number of elements
    int NumAlphas;                   // Number of elements in the alpha array
    int NumNums;                     // Number of elements in the numeric array
    Array1D_string AlphaArray;       // character string data
    Array1D<Real64> NumArray;        // numeric data
    int IOStat;                      // IO Status when calling get input subroutine
    std::string CurrentModuleObject; // for ease in renaming.
    int numFldsToUse;

    auto &elcc(state.dataEconLifeCycleCost);

    if (!elcc->LCCparamPresent) return;
    CurrentModuleObject = "LifeCycleCost:UseAdjustment";
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
    NumArray.allocate(NumNums);
    AlphaArray.allocate(NumAlphas);
    elcc->numUseAdjustment = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    elcc->UseAdjustment.allocate(elcc->numUseAdjustment);
    for (iInObj = 1; iInObj <= elcc->numUseAdjustment; ++iInObj) {
        elcc->UseAdjustment(iInObj).Adjustment.allocate(elcc->lengthStudyYears);
    }
    if (elcc->numUseAdjustment > 0) {
        for (iInObj = 1; iInObj <= elcc->numUseAdjustment; ++iInObj) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     iInObj,
                                                                     AlphaArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            // check to make sure none of the values are another life cycle cost object
            for (jFld = 1; jFld <= NumAlphas; ++jFld) {
                if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                    ShowWarningError(state,
                                     "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                         " a field was found containing LifeCycleCost: which may indicate a missing comma.");
                }
            }
            // start to extract values from input array into appropriate fields
            //  A1,  \field Name
            //       \required-field
            //       \type alpha
            elcc->UseAdjustment(iInObj).name = AlphaArray(1);
            //  A2,  \field Resource
            //       \required-field
            //       \type choice
            //       \key Electricity
            //       \key NaturalGas
            //       \key Steam
            //       \key Gasoline
            //       \key Diesel
            //       \key Coal
            //       \key FuelOilNo1
            //       \key FuelOilNo2
            //       \key Propane
            //       \key Water
            //       \key OtherFuel1
            //       \key OtherFuel2
            elcc->UseAdjustment(iInObj).resource = AssignResourceTypeNum(AlphaArray(2)); // use function from DataGlobalConstants
            if (NumAlphas > 2) {
                ShowWarningError(state, "In " + CurrentModuleObject + " contains more alpha fields than expected.");
            }
            //  N1,  \field Year 1 Multiplier
            //       \type real
            //       \begin-extensible
            // Set the array to default to 1.0
            for (jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
                elcc->UseAdjustment(iInObj).Adjustment(jYear) = 1.0;
            }
            numFldsToUse = min(NumNums, elcc->lengthStudyYears);
            for (jYear = 1; jYear <= numFldsToUse; ++jYear) {
                elcc->UseAdjustment(iInObj).Adjustment(jYear) = NumArray(jYear);
            }
        }
    }
}
//======================================================================================================================
//======================================================================================================================

//    COMPUTATION ROUTINES

//======================================================================================================================
//======================================================================================================================

void ExpressAsCashFlows(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   July 2010
    //    MODIFIED       na
    //    RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //    Convert all recurring and nonrecurring costs into cash flows
    //    used in calculations and reporting.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Using/Aliasing
    using EconomicTariff::GetMonthlyCostForResource;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int iCashFlow;
    int jCost;
    int jAdj;
    int kYear;
    int offset;
    int month; // number of months since base date
    int firstMonth;
    int monthsBaseToService;

    std::map<int, std::map<DataGlobalConstants::ResourceType, Real64>> resourceCosts;
    for (int jMonth = 1; jMonth <= 12; ++jMonth) {
        std::map<DataGlobalConstants::ResourceType, Real64> monthMap;
        for (auto iResource : state.dataGlobalConst->AllResourceTypes) {
            monthMap.insert(std::pair<DataGlobalConstants::ResourceType, Real64>(iResource, 0.0));
        }
        resourceCosts.insert(std::pair<int, std::map<DataGlobalConstants::ResourceType, Real64>>(jMonth, monthMap));
    }

    Array1D<Real64> curResourceCosts(12);

    std::map<DataGlobalConstants::ResourceType, bool> resourceCostNotZero;
    for (auto iResource : state.dataGlobalConst->AllResourceTypes) {
        resourceCostNotZero.insert(std::pair<DataGlobalConstants::ResourceType, bool>(iResource, false));
    }

    std::map<DataGlobalConstants::ResourceType, Real64> resourceCostAnnual;
    for (auto iResource : state.dataGlobalConst->AllResourceTypes) {
        resourceCostAnnual.insert(std::pair<DataGlobalConstants::ResourceType, Real64>(iResource, 0.0));
    }

    Real64 annualCost;
    int found;
    CostCategory curCategory;
    Array1D<Real64> monthlyInflationFactor;
    Real64 inflationPerMonth;
    int iLoop;

    auto &elcc(state.dataEconLifeCycleCost);

    // compute months from 1900 for base and service period
    elcc->ExpressAsCashFlows_baseMonths1900 =
        (elcc->baseDateYear - 1900) * 12 + (elcc->baseDateMonth + 1); // elcc->baseDateMonth + 1 to account for baseDateMonth starting at 0
    elcc->ExpressAsCashFlows_serviceMonths1900 =
        (elcc->serviceDateYear - 1900) * 12 + elcc->serviceDateMonth + 1; // elcc->serviceDateMonth + 1 to account for serviceDateMonth starting at 0
    monthsBaseToService = elcc->ExpressAsCashFlows_serviceMonths1900 - elcc->ExpressAsCashFlows_baseMonths1900;
    // if ComponentCost:LineItem exist, the grand total of all costs are another non-recurring cost
    if (state.dataCostEstimateManager->CurntBldg.GrandTotal >
        0.0) { // from DataCostEstimate and computed in WriteCompCostTable within OutputReportTabular
        ++elcc->numNonrecurringCost;
        elcc->NonrecurringCost[elcc->numNonrecurringCost].name = "Total of ComponentCost:*";
        elcc->NonrecurringCost[elcc->numNonrecurringCost].lineItem = "";
        elcc->NonrecurringCost[elcc->numNonrecurringCost].category = CostCategory::Construction;
        elcc->NonrecurringCost[elcc->numNonrecurringCost].cost = state.dataCostEstimateManager->CurntBldg.GrandTotal;
        elcc->NonrecurringCost[elcc->numNonrecurringCost].startOfCosts = StartCosts::BasePeriod;
        elcc->NonrecurringCost[elcc->numNonrecurringCost].yearsFromStart = 0;
        elcc->NonrecurringCost[elcc->numNonrecurringCost].monthsFromStart = 0;
        elcc->NonrecurringCost[elcc->numNonrecurringCost].totalMonthsFromStart = 0;
    }

    // gather costs from EconomicTariff for each end use
    elcc->numResourcesUsed = 0;
    for (auto iResource : state.dataGlobalConst->AllResourceTypes) {
        GetMonthlyCostForResource(state, iResource, curResourceCosts);
        annualCost = 0.0;
        for (int jMonth = 1; jMonth <= 12; ++jMonth) {
            resourceCosts.at(jMonth).at(iResource) = curResourceCosts(jMonth);
            annualCost += resourceCosts.at(jMonth).at(iResource);
        }
        if (annualCost != 0.0) {
            ++elcc->numResourcesUsed;
            resourceCostNotZero.at(iResource) = true;
        } else {
            resourceCostNotZero.at(iResource) = false;
        }
        resourceCostAnnual.at(iResource) = annualCost;
    }
    // allocate the escalated energy cost arrays
    for (int year = 1; year <= elcc->lengthStudyYears; ++year) {
        std::map<DataGlobalConstants::ResourceType, Real64> yearMap;
        for (auto iResource : state.dataGlobalConst->AllResourceTypes) {
            yearMap.insert(std::pair<DataGlobalConstants::ResourceType, Real64>(iResource, 0.0));
        }
        elcc->EscalatedEnergy.insert(std::pair<int, std::map<DataGlobalConstants::ResourceType, Real64>>(year, yearMap));
    }

    elcc->EscalatedTotEnergy.allocate(elcc->lengthStudyYears);
    elcc->EscalatedTotEnergy = 0.0;

    // pre-compute the inflation factors for each year
    monthlyInflationFactor.allocate(elcc->lengthStudyTotalMonths);
    if (elcc->inflationApproach == InflAppr::ConstantDollar) {
        monthlyInflationFactor = 1.0; // not really used but just in case
    } else if (elcc->inflationApproach == InflAppr::CurrentDollar) {
        // to allocate an interest rate (in this case inflation) cannot just use 1/12
        // for the monthly value since it will be slightly wrong. Instead use inverse of
        // formula from Newnan (4-32) which is r = m x (ia + 1)^(1/m) - 1)
        inflationPerMonth = std::pow(elcc->inflation + 1.0, 1.0 / 12.0) - 1;
        for (int jMonth = 1; jMonth <= elcc->lengthStudyTotalMonths; ++jMonth) {
            monthlyInflationFactor(jMonth) = std::pow(1.0 + inflationPerMonth, jMonth - 1);
        }
    }

    elcc->numCashFlow = CostCategory::Num + elcc->numRecurringCosts + elcc->numNonrecurringCost + elcc->numResourcesUsed;
    // Cashflow array order:
    //   1 cost categories
    //   2 recurring costs
    //   3 nonrecurring costs
    //   4 resource costs
    elcc->CashFlow.resize(elcc->numCashFlow);
    for (iCashFlow = 0; iCashFlow < elcc->numCashFlow; ++iCashFlow) {
        elcc->CashFlow[iCashFlow].mnAmount.allocate(elcc->lengthStudyTotalMonths);
        elcc->CashFlow[iCashFlow].yrAmount.allocate(elcc->lengthStudyYears);
        elcc->CashFlow[iCashFlow].yrPresVal.allocate(elcc->lengthStudyYears);
        elcc->CashFlow[iCashFlow].mnAmount = 0.0;  // zero all cash flow values
        elcc->CashFlow[iCashFlow].yrAmount = 0.0;  // zero all cash flow values
        elcc->CashFlow[iCashFlow].yrPresVal = 0.0; // zero all present values
    }
    // Put nonrecurring costs into cashflows
    offset = CostCategory::Num + elcc->numRecurringCosts;
    for (jCost = 0; jCost < elcc->numNonrecurringCost; ++jCost) {
        elcc->CashFlow[offset + jCost].name = elcc->NonrecurringCost[jCost].name;
        elcc->CashFlow[offset + jCost].SourceKind = SourceKindType::Nonrecurring;
        elcc->CashFlow[offset + jCost].Category = elcc->NonrecurringCost[jCost].category;
        elcc->CashFlow[offset + jCost].orginalCost = elcc->NonrecurringCost[jCost].cost;
        elcc->CashFlow[offset + jCost].mnAmount = 0.0;
        if (elcc->NonrecurringCost[jCost].startOfCosts == StartCosts::ServicePeriod) {
            month = elcc->NonrecurringCost[jCost].totalMonthsFromStart + monthsBaseToService + 1;
        } else if (elcc->NonrecurringCost[jCost].startOfCosts == StartCosts::BasePeriod) {
            month = elcc->NonrecurringCost[jCost].totalMonthsFromStart + 1;
        }
        if ((month >= 1) && (month <= elcc->lengthStudyTotalMonths)) {
            elcc->CashFlow[offset + jCost].mnAmount(month) = elcc->NonrecurringCost[jCost].cost * monthlyInflationFactor(month);
        } else {
            ShowWarningError(state,
                             "For life cycle costing a nonrecurring cost named " + elcc->NonrecurringCost[jCost].name +
                                 " contains a cost which is not within the study period.");
        }
    }
    // Put recurring costs into cashflows
    offset = CostCategory::Num;
    for (jCost = 0; jCost < elcc->numRecurringCosts; ++jCost) {
        elcc->CashFlow[offset + jCost].name = elcc->RecurringCosts[jCost].name;
        elcc->CashFlow[offset + jCost].SourceKind = SourceKindType::Recurring;
        elcc->CashFlow[offset + jCost].Category = elcc->RecurringCosts[jCost].category;
        elcc->CashFlow[offset + jCost].orginalCost = elcc->RecurringCosts[jCost].cost;
        if (elcc->RecurringCosts[jCost].startOfCosts == StartCosts::ServicePeriod) {
            firstMonth = elcc->RecurringCosts[jCost].totalMonthsFromStart + monthsBaseToService + 1;
        } else if (elcc->RecurringCosts[jCost].startOfCosts == StartCosts::BasePeriod) {
            firstMonth = elcc->RecurringCosts[jCost].totalMonthsFromStart + 1;
        }
        if ((firstMonth >= 1) && (firstMonth <= elcc->lengthStudyTotalMonths)) {
            month = firstMonth;
            if (elcc->RecurringCosts[jCost].totalRepeatPeriodMonths >= 1) {
                for (iLoop = 0; iLoop < 10000; ++iLoop) { // add a limit to the loop to prevent runaway condition
                    elcc->CashFlow[offset + jCost].mnAmount(month) = elcc->RecurringCosts[jCost].cost * monthlyInflationFactor(month);
                    month += elcc->RecurringCosts[jCost].totalRepeatPeriodMonths;
                    if (month > elcc->lengthStudyTotalMonths) break;
                }
            }
        } else {
            ShowWarningError(state,
                             "For life cycle costing the recurring cost named " + elcc->RecurringCosts[jCost].name +
                                 " has the first year of the costs that is not within the study period.");
        }
    }
    // Put resource costs into cashflows
    // the first cash flow for resources should be after the categories, recurring and nonrecurring costs
    int cashFlowCounter = CostCategory::Num + elcc->numRecurringCosts + elcc->numNonrecurringCost - 1; // Since CashFlow starts at 0
    for (auto iResource : state.dataGlobalConst->AllResourceTypes) {
        if (resourceCostNotZero.at(iResource)) {
            ++cashFlowCounter;

            switch (iResource) {
            case DataGlobalConstants::ResourceType::Water:
            case DataGlobalConstants::ResourceType::OnSiteWater:
            case DataGlobalConstants::ResourceType::MainsWater:
            case DataGlobalConstants::ResourceType::RainWater:
            case DataGlobalConstants::ResourceType::WellWater:
            case DataGlobalConstants::ResourceType::Condensate:
                elcc->CashFlow[cashFlowCounter].Category = CostCategory::Water;
                break;
            case DataGlobalConstants::ResourceType::Electricity:
            case DataGlobalConstants::ResourceType::Natural_Gas:
            case DataGlobalConstants::ResourceType::Gasoline:
            case DataGlobalConstants::ResourceType::Diesel:
            case DataGlobalConstants::ResourceType::Coal:
            case DataGlobalConstants::ResourceType::FuelOil_1:
            case DataGlobalConstants::ResourceType::FuelOil_2:
            case DataGlobalConstants::ResourceType::Propane:
            case DataGlobalConstants::ResourceType::EnergyTransfer:
            case DataGlobalConstants::ResourceType::Steam:
            case DataGlobalConstants::ResourceType::DistrictCooling:
            case DataGlobalConstants::ResourceType::DistrictHeating:
            case DataGlobalConstants::ResourceType::ElectricityProduced:
            case DataGlobalConstants::ResourceType::ElectricityPurchased:
            case DataGlobalConstants::ResourceType::ElectricityNet:
            case DataGlobalConstants::ResourceType::SolarWater:
            case DataGlobalConstants::ResourceType::SolarAir:
                elcc->CashFlow[cashFlowCounter].Category = CostCategory::Energy;
                break;
            default:
                elcc->CashFlow[cashFlowCounter].Category = CostCategory::Operation;
            }

            elcc->CashFlow[cashFlowCounter].Resource = iResource;
            elcc->CashFlow[cashFlowCounter].SourceKind = SourceKindType::Resource;
            elcc->CashFlow[cashFlowCounter].name = GetResourceTypeChar(iResource);
            if (cashFlowCounter <= elcc->numCashFlow) {
                // put the monthly energy costs into the cashflow prior to adjustments
                // energy costs (a.k.a. resource costs) start at the start of service and repeat
                // until the end of the study total
                for (int jMonth = 1; jMonth <= 12; ++jMonth) {
                    elcc->CashFlow[cashFlowCounter].mnAmount(monthsBaseToService + jMonth) = resourceCosts.at(jMonth).at(iResource);
                }
                elcc->CashFlow[cashFlowCounter].orginalCost = resourceCostAnnual.at(iResource);
                for (int jMonth = monthsBaseToService + 13; jMonth <= elcc->lengthStudyTotalMonths; ++jMonth) {
                    // use the cost from a year earlier
                    elcc->CashFlow[cashFlowCounter].mnAmount(jMonth) = elcc->CashFlow[cashFlowCounter].mnAmount(jMonth - 12);
                }
                // add in the impact of inflation
                for (int jMonth = 1; jMonth <= elcc->lengthStudyTotalMonths; ++jMonth) {
                    elcc->CashFlow[cashFlowCounter].mnAmount(jMonth) *= monthlyInflationFactor(jMonth);
                }
                // now factor in adjustments
                // need to find the correct adjustment to use for the current resource
                found = 0;
                for (jAdj = 1; jAdj <= elcc->numUseAdjustment; ++jAdj) {
                    if (elcc->UseAdjustment(jAdj).resource == iResource) {
                        found = jAdj;
                        break;
                    }
                }
                // if any adjustments were found for that resource apply the multiplier
                if (found != 0) {
                    for (kYear = 1; kYear <= elcc->lengthStudyYears;
                         ++kYear) { // if service period is later than base period then this will go too far
                        for (int jMonth = 1; jMonth <= 12; ++jMonth) {
                            month = (kYear - 1) * 12 + jMonth;
                            if (month > elcc->lengthStudyTotalMonths) break;
                            elcc->CashFlow[cashFlowCounter].mnAmount(month) *= elcc->UseAdjustment(found).Adjustment(kYear);
                        }
                    }
                }
            }
        }
    }
    // put cashflows into categories
    for (jCost = 0; jCost < CostCategory::Num; ++jCost) {
        elcc->CashFlow[jCost].Category = static_cast<CostCategory>(jCost); // make each category the type indicated
        elcc->CashFlow[jCost].SourceKind = SourceKindType::Sum;
    }
    // add the cashflows by category
    for (jCost = CostCategory::Num - 1; jCost < elcc->numCashFlow; ++jCost) {
        curCategory = elcc->CashFlow[jCost].Category;
        if ((curCategory < CostCategory::Num) && (curCategory >= 0)) {
            for (int jMonth = 1; jMonth <= elcc->lengthStudyTotalMonths; ++jMonth) {
                elcc->CashFlow[curCategory].mnAmount(jMonth) += elcc->CashFlow[jCost].mnAmount(jMonth);
            }
        }
    }
    // create total categories
    for (int jMonth = 1; jMonth <= elcc->lengthStudyTotalMonths; ++jMonth) {
        elcc->CashFlow[CostCategory::TotEnergy].mnAmount(jMonth) = elcc->CashFlow[CostCategory::Energy].mnAmount(jMonth);
        elcc->CashFlow[CostCategory::TotOper].mnAmount(jMonth) =
            elcc->CashFlow[CostCategory::Maintenance].mnAmount(jMonth) + elcc->CashFlow[CostCategory::Repair].mnAmount(jMonth) +
            elcc->CashFlow[CostCategory::Operation].mnAmount(jMonth) + elcc->CashFlow[CostCategory::Replacement].mnAmount(jMonth) +
            elcc->CashFlow[CostCategory::MinorOverhaul].mnAmount(jMonth) + elcc->CashFlow[CostCategory::MajorOverhaul].mnAmount(jMonth) +
            elcc->CashFlow[CostCategory::OtherOperational].mnAmount(jMonth) + elcc->CashFlow[CostCategory::Water].mnAmount(jMonth) +
            elcc->CashFlow[CostCategory::Energy].mnAmount(jMonth);
        elcc->CashFlow[CostCategory::TotCaptl].mnAmount(jMonth) = elcc->CashFlow[CostCategory::Construction].mnAmount(jMonth) +
                                                                  elcc->CashFlow[CostCategory::Salvage].mnAmount(jMonth) +
                                                                  elcc->CashFlow[CostCategory::OtherCapital].mnAmount(jMonth);
        elcc->CashFlow[CostCategory::TotGrand].mnAmount(jMonth) =
            elcc->CashFlow[CostCategory::TotOper].mnAmount(jMonth) + elcc->CashFlow[CostCategory::TotCaptl].mnAmount(jMonth);
    }
    // convert all monthly cashflows into yearly cashflows
    for (jCost = 0; jCost < elcc->numCashFlow; ++jCost) {
        for (kYear = 1; kYear <= elcc->lengthStudyYears; ++kYear) {
            annualCost = 0.0;
            for (int jMonth = 1; jMonth <= 12; ++jMonth) {
                month = (kYear - 1) * 12 + jMonth;
                if (month <= elcc->lengthStudyTotalMonths) {
                    annualCost += elcc->CashFlow[jCost].mnAmount(month);
                }
            }
            elcc->CashFlow[jCost].yrAmount(kYear) = annualCost;
        }
    }
    // generate a warning if resource referenced was not used
    for (int nUsePriceEsc = 1; nUsePriceEsc <= elcc->numUsePriceEscalation; ++nUsePriceEsc) {
        auto curResource = elcc->UsePriceEscalation(nUsePriceEsc).resource;
        if (!resourceCostNotZero.at(curResource) && state.dataGlobal->DoWeathSim) {
            ShowWarningError(state,
                             "The resource referenced by LifeCycleCost:UsePriceEscalation= \"" + elcc->UsePriceEscalation(nUsePriceEsc).name +
                                 "\" has no energy cost. ");
            ShowContinueError(state, "... It is likely that the wrong resource is used. The resource should match the meter used in Utility:Tariff.");
        }
    }
}

void ComputeEscalatedEnergyCosts(EnergyPlusData &state)
{
    // J. Glazer - August 2019
    int nUsePriceEsc;

    auto &elcc(state.dataEconLifeCycleCost);

    for (int iCashFlow = 0; iCashFlow < elcc->numCashFlow; ++iCashFlow) {
        if (elcc->CashFlow[iCashFlow].pvKind == PrValKind::Energy) {
            // make sure this is not water
            auto curResource = elcc->CashFlow[iCashFlow].Resource;
            if (elcc->CashFlow[iCashFlow].Resource == DataGlobalConstants::ResourceType::Water ||
                (elcc->CashFlow[iCashFlow].Resource >= DataGlobalConstants::ResourceType::OnSiteWater &&
                 elcc->CashFlow[iCashFlow].Resource <= DataGlobalConstants::ResourceType::Condensate)) {
                continue;
            }
            if ((curResource != DataGlobalConstants::ResourceType::None)) {
                int found = 0;
                for (nUsePriceEsc = 1; nUsePriceEsc <= elcc->numUsePriceEscalation; ++nUsePriceEsc) {
                    if (elcc->UsePriceEscalation(nUsePriceEsc).resource == curResource) {
                        found = nUsePriceEsc;
                        break;
                    }
                }
                if (found > 0) {
                    for (int jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
                        elcc->EscalatedEnergy.at(jYear).at(curResource) =
                            elcc->CashFlow[iCashFlow].yrAmount(jYear) * elcc->UsePriceEscalation(found).Escalation(jYear);
                    }
                } else { // if no escalation than just store the original energy cost
                    for (int jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
                        elcc->EscalatedEnergy.at(jYear).at(curResource) = elcc->CashFlow[iCashFlow].yrAmount(jYear);
                    }
                }
            }
        }
    }
    for (auto kResource : state.dataGlobalConst->AllResourceTypes) {
        for (int jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
            elcc->EscalatedTotEnergy(jYear) += elcc->EscalatedEnergy.at(jYear).at(kResource);
        }
    }
}

void ComputePresentValue(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   August 2010
    //    MODIFIED       na
    //    RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //    For each cashflow, compute the present value

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 totalPV;
    int curCategory;
    Real64 curDiscountRate;
    int iCashFlow;
    int jYear;
    int nUsePriceEsc;
    Real64 effectiveYear;

    auto &elcc(state.dataEconLifeCycleCost);

    // identify how each cashflow should be treated
    for (iCashFlow = 0; iCashFlow < elcc->numCashFlow; ++iCashFlow) {
        switch (elcc->CashFlow[iCashFlow].SourceKind) {
        case (SourceKindType::Resource): {
            // only for real fuels purchased such as electricity, natural gas, etc..
            if ((elcc->CashFlow[iCashFlow].Resource >= DataGlobalConstants::ResourceType::Electricity) &&
                (elcc->CashFlow[iCashFlow].Resource <= DataGlobalConstants::ResourceType::ElectricitySurplusSold)) {
                elcc->CashFlow[iCashFlow].pvKind = PrValKind::Energy;
            } else {
                elcc->CashFlow[iCashFlow].pvKind = PrValKind::NonEnergy;
            }
            break;
        }
        case (SourceKindType::Recurring):
        case (SourceKindType::Nonrecurring): {
            if (elcc->CashFlow[iCashFlow].Category == CostCategory::Energy) {
                elcc->CashFlow[iCashFlow].pvKind = PrValKind::Energy;
            } else {
                elcc->CashFlow[iCashFlow].pvKind = PrValKind::NonEnergy;
            }
            break;
        }
        case (SourceKindType::Sum):
        default: {
            elcc->CashFlow[iCashFlow].pvKind = PrValKind::NotComputed;
            break;
        }
        }
    }
    // compute the Single Present Value factors based on the discount rate
    elcc->SPV.allocate(elcc->lengthStudyYears);
    for (int year = 1; year <= elcc->lengthStudyYears; ++year) {
        std::map<DataGlobalConstants::ResourceType, Real64> yearMap;
        for (auto iResource : state.dataGlobalConst->AllResourceTypes) {
            yearMap.insert(std::pair<DataGlobalConstants::ResourceType, Real64>(iResource, 0.0));
        }
        elcc->energySPV.insert(std::pair<int, std::map<DataGlobalConstants::ResourceType, Real64>>(year, yearMap));
    }

    // Depending if using Constant or Current Dollar analysis
    // use the appropriate discount rate
    if (elcc->inflationApproach == InflAppr::ConstantDollar) {
        curDiscountRate = elcc->realDiscountRate;
    } else if (elcc->inflationApproach == InflAppr::CurrentDollar) {
        curDiscountRate = elcc->nominalDiscountRate;
    }
    // compute single present values based on real discount rates
    constexpr std::array<Real64, static_cast<int>(DiscConv::Num)> DiscConv2EffectiveYearAdjustment = {1.0, 0.5, 0.0};
    for (jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
        // NIST 155 D.2.1.1 - Single Present Value (SPV) formula
        effectiveYear = double(jYear) - DiscConv2EffectiveYearAdjustment[static_cast<int>(elcc->discountConvention)];
        elcc->SPV(jYear) = 1.0 / std::pow(1.0 + curDiscountRate, effectiveYear);
    }
    // use SPV as default values for all energy types
    for (jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
        for (auto kResource : state.dataGlobalConst->AllResourceTypes) {
            elcc->energySPV.at(jYear).at(kResource) = elcc->SPV(jYear);
        }
    }
    // loop through the resources and if they match a UseEscalation use those values instead
    for (nUsePriceEsc = 1; nUsePriceEsc <= elcc->numUsePriceEscalation; ++nUsePriceEsc) {
        auto curResource = elcc->UsePriceEscalation(nUsePriceEsc).resource;
        if (curResource != DataGlobalConstants::ResourceType::None) {
            for (jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
                // the following is based on UPV* formula from NIST 135 supplement but is for a single year
                effectiveYear = double(jYear) - DiscConv2EffectiveYearAdjustment[static_cast<int>(elcc->discountConvention)];
                elcc->energySPV.at(jYear).at(curResource) =
                    elcc->UsePriceEscalation(nUsePriceEsc).Escalation(jYear) / std::pow(1.0 + curDiscountRate, effectiveYear);
            }
        }
    }
    for (iCashFlow = 0; iCashFlow < elcc->numCashFlow; ++iCashFlow) {
        switch (elcc->CashFlow[iCashFlow].pvKind) {
        case (PrValKind::NonEnergy): {
            totalPV = 0.0;
            for (jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
                elcc->CashFlow[iCashFlow].yrPresVal(jYear) = elcc->CashFlow[iCashFlow].yrAmount(jYear) * elcc->SPV(jYear);
                totalPV += elcc->CashFlow[iCashFlow].yrPresVal(jYear);
            }
            elcc->CashFlow[iCashFlow].presentValue = totalPV;
            break;
        }
        case (PrValKind::Energy): {
            auto curResource = elcc->CashFlow[iCashFlow].Resource;
            if (curResource != DataGlobalConstants::ResourceType::None) {
                totalPV = 0.0;
                for (jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
                    elcc->CashFlow[iCashFlow].yrPresVal(jYear) =
                        elcc->CashFlow[iCashFlow].yrAmount(jYear) * elcc->energySPV.at(jYear).at(curResource);
                    totalPV += elcc->CashFlow[iCashFlow].yrPresVal(jYear);
                }
                elcc->CashFlow[iCashFlow].presentValue = totalPV;
            }
            break;
        }
            //            case (iPrValKind::NotComputed): included in default
        default:
            break; // do nothing
        }
    }
    // sum by category
    for (int i = 0; i < CostCategory::Num; ++i) {
        elcc->CashFlow[i].presentValue = 0; // initialize value to zero before summing in next for loop
    }
    for (iCashFlow = CostCategory::Num; iCashFlow < elcc->numCashFlow; ++iCashFlow) {
        curCategory = elcc->CashFlow[iCashFlow].Category;
        if ((curCategory < CostCategory::Num) && (curCategory >= 0)) {
            elcc->CashFlow[curCategory].presentValue += elcc->CashFlow[iCashFlow].presentValue;
            for (jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
                elcc->CashFlow[curCategory].yrPresVal(jYear) += elcc->CashFlow[iCashFlow].yrPresVal(jYear);
            }
        }
    }
    // create total categories
    elcc->CashFlow[CostCategory::TotEnergy].presentValue = elcc->CashFlow[CostCategory::Energy].presentValue;
    elcc->CashFlow[CostCategory::TotOper].presentValue =
        elcc->CashFlow[CostCategory::Maintenance].presentValue + elcc->CashFlow[CostCategory::Repair].presentValue +
        elcc->CashFlow[CostCategory::Operation].presentValue + elcc->CashFlow[CostCategory::Replacement].presentValue +
        elcc->CashFlow[CostCategory::MinorOverhaul].presentValue + elcc->CashFlow[CostCategory::MajorOverhaul].presentValue +
        elcc->CashFlow[CostCategory::OtherOperational].presentValue + elcc->CashFlow[CostCategory::Water].presentValue +
        elcc->CashFlow[CostCategory::Energy].presentValue;
    elcc->CashFlow[CostCategory::TotCaptl].presentValue = elcc->CashFlow[CostCategory::Construction].presentValue +
                                                          elcc->CashFlow[CostCategory::Salvage].presentValue +
                                                          elcc->CashFlow[CostCategory::OtherCapital].presentValue;
    elcc->CashFlow[CostCategory::TotGrand].presentValue =
        elcc->CashFlow[CostCategory::TotOper].presentValue + elcc->CashFlow[CostCategory::TotCaptl].presentValue;
    for (jYear = 1; jYear <= elcc->lengthStudyYears; ++jYear) {
        elcc->CashFlow[CostCategory::TotEnergy].yrPresVal(jYear) = elcc->CashFlow[CostCategory::Energy].yrPresVal(jYear);
        elcc->CashFlow[CostCategory::TotOper].yrPresVal(jYear) =
            elcc->CashFlow[CostCategory::Maintenance].yrPresVal(jYear) + elcc->CashFlow[CostCategory::Repair].yrPresVal(jYear) +
            elcc->CashFlow[CostCategory::Operation].yrPresVal(jYear) + elcc->CashFlow[CostCategory::Replacement].yrPresVal(jYear) +
            elcc->CashFlow[CostCategory::MinorOverhaul].yrPresVal(jYear) + elcc->CashFlow[CostCategory::MajorOverhaul].yrPresVal(jYear) +
            elcc->CashFlow[CostCategory::OtherOperational].yrPresVal(jYear) + elcc->CashFlow[CostCategory::Water].yrPresVal(jYear) +
            elcc->CashFlow[CostCategory::Energy].yrPresVal(jYear);
        elcc->CashFlow[CostCategory::TotCaptl].yrPresVal(jYear) = elcc->CashFlow[CostCategory::Construction].yrPresVal(jYear) +
                                                                  elcc->CashFlow[CostCategory::Salvage].yrPresVal(jYear) +
                                                                  elcc->CashFlow[CostCategory::OtherCapital].yrPresVal(jYear);
        elcc->CashFlow[CostCategory::TotGrand].yrPresVal(jYear) =
            elcc->CashFlow[CostCategory::TotOper].yrPresVal(jYear) + elcc->CashFlow[CostCategory::TotCaptl].yrPresVal(jYear);
    }
}

void ComputeTaxAndDepreciation(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   August 2010
    //    MODIFIED       na
    //    RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //    Compute the present value after factoring in taxes
    //    and depreciation.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 curCapital;
    int curDepYear;
    int iYear;
    int jYear;

    auto &elcc(state.dataEconLifeCycleCost);

    elcc->DepreciatedCapital.allocate(elcc->lengthStudyYears);
    elcc->TaxableIncome.allocate(elcc->lengthStudyYears);
    elcc->Taxes.allocate(elcc->lengthStudyYears);
    elcc->AfterTaxCashFlow.allocate(elcc->lengthStudyYears);
    elcc->AfterTaxPresentValue.allocate(elcc->lengthStudyYears);

    // Depreciation factors are based on IRS Publication 946 for 2009 "How to Depreciate Property"
    // The MACRS valus are based on Modified Accelerated Cost Recovery System GDS for 3, 5, 7, 10 year
    // property are based on 200% depreciation method shown in Appendix A using half year. 15 and 20 are
    // based on 150% (Chart 1). For Straight Line depreciation GDS is used for 27 years (actually 27.5)
    // 31 years (actually 31.5 years) and 39 years using mid month. For 40 years ADS is used (chart 2)
    // Table A-1 is used for 3, 4, 5, 10, 15 and 20 years. Table A-6 is for 27 years. Table A-7 for 31 years.
    // Table A-7a for 39 years. Table A-13 for 40 years. These years are a classification of property
    // and should not be confused with the length of the study. For 27 years, 31 years, 39 years and 40 years
    // the June value was used.

    // convert construction costs (not salvage) into depreciation
    elcc->DepreciatedCapital = 0.0; // set all years to zero
    for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
        curCapital = elcc->CashFlow[CostCategory::Construction].yrAmount(iYear) + elcc->CashFlow[CostCategory::OtherCapital].yrAmount(iYear);
        for (jYear = 0; jYear < SizeDepr; ++jYear) {
            curDepYear = iYear + jYear; // start depreciating with the year that the capital was shown and go to years following
            if (curDepYear <= elcc->lengthStudyYears) {
                elcc->DepreciatedCapital(curDepYear) +=
                    curCapital * (DepreciationPercentTable[static_cast<int>(elcc->depreciationMethod)][jYear] / 100);
            }
        }
    }
    // Using Newnan pg 3880
    //   before-tax cash flow
    //   depreciation
    //   taxable income (before-tax cash flow - depreciation)
    //   income taxes (taxable income x incremental tax rate)
    //   after-tax cash flow (before-tax cash flow - income taxes)
    for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
        elcc->TaxableIncome(iYear) = elcc->CashFlow[CostCategory::TotGrand].yrAmount(iYear) - elcc->DepreciatedCapital(iYear);
        elcc->Taxes(iYear) = elcc->TaxableIncome(iYear) * elcc->taxRate;
        elcc->AfterTaxCashFlow(iYear) = elcc->CashFlow[CostCategory::TotGrand].yrAmount(iYear) - elcc->Taxes(iYear);
        // the present value after taxes is pretax present value minus the present value of the taxes
        elcc->AfterTaxPresentValue(iYear) = elcc->CashFlow[CostCategory::TotGrand].yrPresVal(iYear) - elcc->Taxes(iYear) * elcc->SPV(iYear);
    }
}

//======================================================================================================================
//======================================================================================================================

//    OUTPUT ROUTINES

//======================================================================================================================
//======================================================================================================================

void WriteTabularLifeCycleCostReport(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //    AUTHOR         Jason Glazer of GARD Analytics, Inc.
    //    DATE WRITTEN   June 2010
    //    MODIFIED       na
    //    RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //    Write the output report related to life-cycle costing
    //    to the tabular output file.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // na

    // Using/Aliasing
    using OutputReportTabular::RealToStr;
    using OutputReportTabular::WriteReportHeaders;
    using OutputReportTabular::WriteSubtitle;
    using OutputReportTabular::WriteTable;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    // all arrays are in the format: (row, column)
    Array1D_string columnHead;
    Array1D_int columnWidth;
    Array1D_string rowHead;
    Array2D_string tableBody;

    int numColumns;
    int iYear;
    int jObj;
    int kMonth;
    int curCashFlow;
    int numRows;
    int offset;
    int numYears;
    Real64 totalPV;

    auto &elcc(state.dataEconLifeCycleCost);

    if (elcc->LCCparamPresent && state.dataOutRptTab->displayLifeCycleCostReport) {
        //---------------------------------
        // Life-Cycle Cost Verification and Results Report
        //---------------------------------
        WriteReportHeaders(state, "Life-Cycle Cost Report", "Entire Facility", OutputProcessor::StoreType::Averaged);
        //---- Life-Cycle Cost Parameters
        rowHead.allocate(11);
        columnHead.allocate(1);
        columnWidth.allocate(1);
        tableBody.allocate(1, 11);
        tableBody = "";
        rowHead(1) = "Name";
        rowHead(2) = "Discounting Convention";
        rowHead(3) = "Inflation Approach";
        rowHead(4) = "Real Discount Rate";
        rowHead(5) = "Nominal Discount Rate";
        rowHead(6) = "Inflation";
        rowHead(7) = "Base Date";
        rowHead(8) = "Service Date";
        rowHead(9) = "Length of Study Period in Years";
        rowHead(10) = "Tax rate";
        rowHead(11) = "Depreciation Method";
        columnHead(1) = "Value";

        tableBody(1, 1) = elcc->LCCname;
        tableBody(1, 2) = DiscConvNames[static_cast<int>(elcc->discountConvention)];
        tableBody(1, 3) = InflApprNames[static_cast<int>(elcc->inflationApproach)];
        if (elcc->inflationApproach == InflAppr::ConstantDollar) {
            tableBody(1, 4) = RealToStr(elcc->realDiscountRate, 4);
        } else {
            tableBody(1, 4) = "-- N/A --";
        }
        if (elcc->inflationApproach == InflAppr::CurrentDollar) {
            tableBody(1, 5) = RealToStr(elcc->nominalDiscountRate, 4);
        } else {
            tableBody(1, 5) = "-- N/A --";
        }
        if (elcc->inflationApproach == InflAppr::CurrentDollar) {
            tableBody(1, 6) = RealToStr(elcc->inflation, 4);
        } else {
            tableBody(1, 6) = "-- N/A --";
        }
        tableBody(1, 7) = format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->baseDateMonth)], elcc->baseDateYear);
        tableBody(1, 8) = format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->serviceDateMonth)], elcc->serviceDateYear);
        tableBody(1, 9) = fmt::to_string(elcc->lengthStudyYears);
        tableBody(1, 10) = RealToStr(elcc->taxRate, 4);
        tableBody(1, 11) = DeprMethodNames[static_cast<int>(elcc->depreciationMethod)];

        columnWidth = 14; // array assignment - same for all columns
        WriteSubtitle(state, "Life-Cycle Cost Parameters");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Life-Cycle Cost Parameters");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Life-Cycle Cost Parameters");
        }

        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- Use Price Escalation
        numColumns = max(1, elcc->numUsePriceEscalation);
        rowHead.allocate(elcc->lengthStudyYears + 2);
        columnHead.allocate(numColumns);
        columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
        tableBody.allocate(numColumns, elcc->lengthStudyYears + 2);
        tableBody = "";
        columnHead = "none";
        rowHead(1) = "Resource";
        rowHead(2) = "Start Date";
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            rowHead(iYear + 2) = fmt::to_string(iYear);
        }
        for (jObj = 1; jObj <= elcc->numUsePriceEscalation; ++jObj) { // loop through objects not columns to add names
            columnHead(jObj) = elcc->UsePriceEscalation(jObj).name;
            tableBody(jObj, 1) = GetResourceTypeChar(elcc->UsePriceEscalation(jObj).resource);
            tableBody(jObj, 2) = format("{} {}",
                                        UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->UsePriceEscalation(jObj).escalationStartMonth)],
                                        elcc->UsePriceEscalation(jObj).escalationStartYear);
        }
        for (jObj = 1; jObj <= elcc->numUsePriceEscalation; ++jObj) {
            for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
                tableBody(jObj, iYear + 2) = RealToStr(elcc->UsePriceEscalation(jObj).Escalation(iYear), 6);
            }
        }
        WriteSubtitle(state, "Use Price Escalation");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Price Escalation");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Price Escalation");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- Use Adjustment
        if (elcc->numUseAdjustment >= 1) { // only create table if objects used
            numColumns = max(1, elcc->numUseAdjustment);
            numYears = elcc->lengthStudyYears - (elcc->serviceDateYear - elcc->baseDateYear);
            rowHead.allocate(numYears + 1);
            columnHead.allocate(numColumns);
            columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
            tableBody.allocate(numColumns, numYears + 1);
            tableBody = "";
            columnHead = "none";
            rowHead(1) = "";
            for (iYear = 1; iYear <= numYears; ++iYear) {
                rowHead(iYear + 1) =
                    format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->serviceDateMonth)], elcc->serviceDateYear + iYear - 1);
            }
            for (jObj = 1; jObj <= elcc->numUseAdjustment; ++jObj) { // loop through objects not columns to add names
                columnHead(jObj) = elcc->UseAdjustment(jObj).name;
                tableBody(jObj, 1) = GetResourceTypeChar(elcc->UseAdjustment(jObj).resource);
            }
            for (jObj = 1; jObj <= elcc->numUseAdjustment; ++jObj) {
                for (iYear = 1; iYear <= numYears; ++iYear) {
                    tableBody(jObj, iYear + 1) = RealToStr(elcc->UseAdjustment(jObj).Adjustment(iYear), 6);
                }
            }
            WriteSubtitle(state, "Use Adjustment");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Adjustment");
            }
            if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
                state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Adjustment");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
        }
        //---- Cash Flow for Recurring and Nonrecurring Costs
        numColumns = max(1, elcc->numRecurringCosts + elcc->numNonrecurringCost);
        rowHead.allocate(elcc->lengthStudyYears + 1);
        columnHead.allocate(numColumns);
        columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
        tableBody.allocate(numColumns, elcc->lengthStudyYears + 1);
        tableBody = "";
        rowHead(1) = "";
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            rowHead(iYear + 1) =
                format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->baseDateMonth)], elcc->baseDateYear + iYear - 1);
        }
        for (jObj = 0; jObj < (elcc->numRecurringCosts + elcc->numNonrecurringCost); ++jObj) {
            curCashFlow = CostCategory::Num + jObj;
            columnHead(jObj + 1) = elcc->CashFlow[curCashFlow].name;

            tableBody(jObj + 1, 1) = SourceKindTypeNames[static_cast<int>(elcc->CashFlow[curCashFlow].SourceKind)];

            for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
                tableBody(jObj + 1, iYear + 1) = RealToStr(elcc->CashFlow[curCashFlow].yrAmount(iYear), 2);
            }
        }
        WriteSubtitle(state, "Cash Flow for Recurring and Nonrecurring Costs (Without Escalation)");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(tableBody,
                                                                               rowHead,
                                                                               columnHead,
                                                                               "Life-Cycle Cost Report",
                                                                               "Entire Facility",
                                                                               "Cash Flow for Recurring and Nonrecurring Costs (Without Escalation)");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody,
                rowHead,
                columnHead,
                "Life-Cycle Cost Report",
                "Entire Facility",
                "Cash Flow for Recurring and Nonrecurring Costs (Without Escalation)");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- Energy and Water Cost Cash Flows (Without Escalation)
        numColumns = max(1, elcc->numResourcesUsed + 1);
        rowHead.allocate(elcc->lengthStudyYears);
        columnHead.allocate(numColumns);
        columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
        tableBody.allocate(numColumns, elcc->lengthStudyYears);
        tableBody = "";
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            rowHead(iYear) = format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->baseDateMonth)], elcc->baseDateYear + iYear - 1);
        }
        for (jObj = 0; jObj < elcc->numResourcesUsed; ++jObj) {
            curCashFlow = CostCategory::Num + elcc->numRecurringCosts + elcc->numNonrecurringCost + jObj;
            columnHead(jObj + 1) = elcc->CashFlow[curCashFlow].name;
            for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
                tableBody(jObj + 1, iYear) = RealToStr(elcc->CashFlow[curCashFlow].yrAmount(iYear), 2);
            }
        }
        columnHead(numColumns) = "Total";
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            tableBody(jObj + 1, iYear) =
                RealToStr(elcc->CashFlow[CostCategory::TotEnergy].yrAmount(iYear) + elcc->CashFlow[CostCategory::Water].yrAmount(iYear), 2);
        }
        WriteSubtitle(state, "Energy and Water Cost Cash Flows (Without Escalation)");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Energy and Water Cost Cash Flows (Without Escalation)");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Energy and Water Cost Cash Flows (Without Escalation)");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- Energy and Water Cost Cash Flows (With Escalation)
        numColumns = max(1, elcc->numResourcesUsed + 1);
        rowHead.allocate(elcc->lengthStudyYears);
        columnHead.allocate(numColumns);
        columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
        tableBody.allocate(numColumns, elcc->lengthStudyYears);
        tableBody = "";
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            rowHead(iYear) = format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->baseDateMonth)], elcc->baseDateYear + iYear - 1);
        }
        for (jObj = 0; jObj < elcc->numResourcesUsed; ++jObj) {
            curCashFlow = CostCategory::Num + elcc->numRecurringCosts + elcc->numNonrecurringCost + jObj;
            columnHead(jObj + 1) = elcc->CashFlow[curCashFlow].name;
            auto curResource = elcc->CashFlow[curCashFlow].Resource;
            if (elcc->CashFlow[curCashFlow].Resource != DataGlobalConstants::ResourceType::Water) {
                for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
                    tableBody(jObj + 1, iYear) = RealToStr(elcc->EscalatedEnergy.at(iYear).at(curResource), 2);
                }
            } else { // for water just use the original cashflow since not involved in escalation
                for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
                    tableBody(jObj + 1, iYear) = RealToStr(elcc->CashFlow[curCashFlow].yrAmount(iYear), 2);
                }
            }
        }
        columnHead(numColumns) = "Total";
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            tableBody(jObj + 1, iYear) = RealToStr(elcc->EscalatedTotEnergy(iYear) + elcc->CashFlow[CostCategory::Water].yrAmount(iYear), 2);
        }
        WriteSubtitle(state, "Energy and Water Cost Cash Flows (With Escalation)");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Energy and Water Cost Cash Flows (With Escalation)");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Energy and Water Cost Cash Flows (With Escalation)");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();

        //---- Capital Cash Flow by Category
        rowHead.allocate(elcc->lengthStudyYears);
        columnHead.allocate(4);
        columnWidth.allocate(4);
        columnWidth = 14; // array assignment - same for all columns
        tableBody.allocate(4, elcc->lengthStudyYears);
        tableBody = "";
        columnHead(1) = CostCategoryNames[static_cast<int>(CostCategory::Construction)];
        columnHead(2) = CostCategoryNames[static_cast<int>(CostCategory::Salvage)];
        columnHead(3) = CostCategoryNames[static_cast<int>(CostCategory::OtherCapital)];
        columnHead(4) = "Total";
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            rowHead(iYear) = format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->baseDateMonth)], elcc->baseDateYear + iYear - 1);
            tableBody(1, iYear) = RealToStr(elcc->CashFlow[CostCategory::Construction].yrAmount(iYear), 2);
            tableBody(2, iYear) = RealToStr(elcc->CashFlow[CostCategory::Salvage].yrAmount(iYear), 2);
            tableBody(3, iYear) = RealToStr(elcc->CashFlow[CostCategory::OtherCapital].yrAmount(iYear), 2);
            tableBody(4, iYear) = RealToStr(elcc->CashFlow[CostCategory::TotCaptl].yrAmount(iYear), 2);
        }
        WriteSubtitle(state, "Capital Cash Flow by Category (Without Escalation)");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Capital Cash Flow by Category (Without Escalation)");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Capital Cash Flow by Category (Without Escalation)");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- Operating Cash Flow by Category (Without Escalation)
        rowHead.allocate(elcc->lengthStudyYears);
        columnHead.allocate(10);
        columnWidth.allocate(10);
        columnWidth = 14; // array assignment - same for all columns
        tableBody.allocate(10, elcc->lengthStudyYears);
        tableBody = "";
        columnHead(1) = "Energy";
        columnHead(2) = "Water";
        columnHead(3) = "Maintenance";
        columnHead(4) = "Repair";
        columnHead(5) = "Operation";
        columnHead(6) = "Replacement";
        columnHead(7) = "MinorOverhaul";
        columnHead(8) = "MajorOverhaul";
        columnHead(9) = "OtherOperational";
        columnHead(10) = "Total";
//        columnHead(1) = "Maintenance";
//        columnHead(2) = "Repair";
//        columnHead(3) = "Operation";
//        columnHead(4) = "Replacement";
//        columnHead(5) = "MinorOverhaul";
//        columnHead(6) = "MajorOverhaul";
//        columnHead(7) = "OtherOperational";
//        columnHead(8) = "Energy";
//        columnHead(9) = "Water";
//        columnHead(10) = "Total";

        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            rowHead(iYear) = format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->baseDateMonth)], elcc->baseDateYear + iYear - 1);
            tableBody(1, iYear) = RealToStr(elcc->CashFlow[CostCategory::Energy].yrAmount(iYear), 2);
            tableBody(2, iYear) = RealToStr(elcc->CashFlow[CostCategory::Water].yrAmount(iYear), 2);
            tableBody(3, iYear) = RealToStr(elcc->CashFlow[CostCategory::Maintenance].yrAmount(iYear), 2);
            tableBody(4, iYear) = RealToStr(elcc->CashFlow[CostCategory::Repair].yrAmount(iYear), 2);
            tableBody(5, iYear) = RealToStr(elcc->CashFlow[CostCategory::Operation].yrAmount(iYear), 2);
            tableBody(6, iYear) = RealToStr(elcc->CashFlow[CostCategory::Replacement].yrAmount(iYear), 2);
            tableBody(7, iYear) = RealToStr(elcc->CashFlow[CostCategory::MinorOverhaul].yrAmount(iYear), 2);
            tableBody(8, iYear) = RealToStr(elcc->CashFlow[CostCategory::MajorOverhaul].yrAmount(iYear), 2);
            tableBody(9, iYear) = RealToStr(elcc->CashFlow[CostCategory::OtherOperational].yrAmount(iYear), 2);
            tableBody(10, iYear) = RealToStr(elcc->CashFlow[CostCategory::TotOper].yrAmount(iYear), 2);
        }
        WriteSubtitle(state, "Operating Cash Flow by Category (Without Escalation)");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Operating Cash Flow by Category (Without Escalation)");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Operating Cash Flow by Category (Without Escalation)");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- Operating Cash Flow by Category (With Escalation)
        rowHead.allocate(elcc->lengthStudyYears);
        columnHead.allocate(10);
        columnWidth.allocate(10);
        columnWidth = 14; // array assignment - same for all columns
        tableBody.allocate(10, elcc->lengthStudyYears);
        tableBody = "";
        columnHead(1) = "Energy";
        columnHead(2) = "Water";
        columnHead(3) = "Maintenance";
        columnHead(4) = "Repair";
        columnHead(5) = "Operation";
        columnHead(6) = "Replacement";
        columnHead(7) = "MinorOverhaul";
        columnHead(8) = "MajorOverhaul";
        columnHead(9) = "OtherOperational";
        columnHead(10) = "Total";

        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            rowHead(iYear) = format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->baseDateMonth)], elcc->baseDateYear + iYear - 1);
            tableBody(1, iYear) = RealToStr(elcc->EscalatedTotEnergy(iYear), 2);
            tableBody(2, iYear) = RealToStr(elcc->CashFlow[CostCategory::Water].yrAmount(iYear), 2);
            tableBody(3, iYear) = RealToStr(elcc->CashFlow[CostCategory::Maintenance].yrAmount(iYear), 2);
            tableBody(4, iYear) = RealToStr(elcc->CashFlow[CostCategory::Repair].yrAmount(iYear), 2);
            tableBody(5, iYear) = RealToStr(elcc->CashFlow[CostCategory::Operation].yrAmount(iYear), 2);
            tableBody(6, iYear) = RealToStr(elcc->CashFlow[CostCategory::Replacement].yrAmount(iYear), 2);
            tableBody(7, iYear) = RealToStr(elcc->CashFlow[CostCategory::MinorOverhaul].yrAmount(iYear), 2);
            tableBody(8, iYear) = RealToStr(elcc->CashFlow[CostCategory::MajorOverhaul].yrAmount(iYear), 2);
            tableBody(9, iYear) = RealToStr(elcc->CashFlow[CostCategory::OtherOperational].yrAmount(iYear), 2);
            Real64 yearly_total_cost = elcc->CashFlow[CostCategory::TotOper].yrAmount(iYear) + elcc->EscalatedTotEnergy(iYear) -
                                       elcc->CashFlow[CostCategory::TotEnergy].yrAmount(iYear);
            tableBody(10, iYear) = RealToStr(yearly_total_cost, 2);
        }
        WriteSubtitle(state, "Operating Cash Flow by Category (With Escalation)");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Operating Cash Flow by Category (With Escalation)");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Operating Cash Flow by Category (With Escalation)");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- DEBUG ONLY - Monthly Cash Flows
        bool showMonthlyCashFlows = false;
        if (showMonthlyCashFlows) {
            rowHead.allocate(elcc->lengthStudyTotalMonths);
            columnHead.allocate(elcc->numCashFlow);
            columnWidth.allocate(elcc->numCashFlow);
            tableBody.allocate(elcc->numCashFlow, elcc->lengthStudyTotalMonths);
            tableBody = "";
            columnHead(1) = "mnt";
            columnHead(2) = "rpr";
            columnHead(3) = "opr";
            columnHead(4) = "repl";
            columnHead(5) = "mOvhl";
            columnHead(6) = "MOvhl";
            columnHead(7) = "oOpr";
            columnHead(8) = "cons";
            columnHead(9) = "slvg";
            columnHead(10) = "oCap";
            columnHead(11) = "H2O";
            columnHead(12) = "ene";
            columnHead(13) = "tEne";
            columnHead(14) = "tOpr";
            columnHead(15) = "tCap";
            columnHead(16) = "Totl";
            for (jObj = CostCategory::Num; jObj < elcc->numCashFlow; ++jObj) {
                columnHead(jObj + 1) = elcc->CashFlow[jObj].name;
            }
            for (kMonth = 1; kMonth <= elcc->lengthStudyTotalMonths; ++kMonth) {
                rowHead(kMonth) = format("{} {}",
                                         UtilityRoutines::MonthNamesCC[static_cast<int>(1 + (kMonth + elcc->baseDateMonth - 2) % 12) - 1],
                                         elcc->baseDateYear + int((kMonth - 1) / 12));
            }
            for (kMonth = 1; kMonth <= elcc->lengthStudyTotalMonths; ++kMonth) {
                for (jObj = 0; jObj < elcc->numCashFlow; ++jObj) {
                    tableBody(jObj + 1, kMonth) = RealToStr(elcc->CashFlow[jObj].mnAmount(kMonth), 2);
                }
            }
            WriteSubtitle(state, "DEBUG ONLY - Monthly Cash Flows");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "DEBUG ONLY - Monthly Cash Flows");
            }
            if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
                state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "DEBUG ONLY - Monthly Cash Flows");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
        }
        //---- Monthly Total Cash Flow
        rowHead.allocate(elcc->lengthStudyYears);
        columnHead.allocate(12);
        columnWidth.allocate(12);
        columnWidth = 14; // array assignment - same for all columns
        tableBody.allocate(12, elcc->lengthStudyYears);
        tableBody = "";
        for (kMonth = 1; kMonth <= 12; ++kMonth) {
            columnHead(kMonth) = UtilityRoutines::MonthNamesCC[static_cast<int>(kMonth - 1)];
        }
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            rowHead(iYear) = fmt::to_string(elcc->baseDateYear + iYear - 1);
        }
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            for (kMonth = 1; kMonth <= 12; ++kMonth) {
                tableBody(kMonth, iYear) = RealToStr(elcc->CashFlow[CostCategory::TotGrand].mnAmount((iYear - 1) * 12 + kMonth), 2);
            }
        }
        WriteSubtitle(state, "Monthly Total Cash Flow (Without Escalation)");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Monthly Total Cash Flow (Without Escalation)");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Monthly Total Cash Flow (Without Escalation)");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- Present Value for Recurring, Nonrecurring and Energy Costs
        numRows = max(1, elcc->numRecurringCosts + elcc->numNonrecurringCost + elcc->numResourcesUsed);
        rowHead.allocate(numRows + 1);
        columnHead.allocate(5);
        columnWidth.allocate(5);
        columnWidth = 14; // array assignment - same for all columns
        tableBody.allocate(5, numRows + 1);
        tableBody = "";
        columnHead(1) = "Category";
        columnHead(2) = "Kind";
        columnHead(3) = "Cost";
        columnHead(4) = "Present Value";
        columnHead(5) = "Present Value Factor";
        totalPV = 0.0;
        rowHead(numRows + 1) = "TOTAL";
        for (jObj = 0; jObj < (elcc->numRecurringCosts + elcc->numNonrecurringCost + elcc->numResourcesUsed); ++jObj) {
            offset = CostCategory::Num;
            rowHead(jObj + 1) = elcc->CashFlow[offset + jObj].name;
            switch (elcc->CashFlow[offset + jObj].Category) {
            case (CostCategory::Maintenance):
            case (CostCategory::Repair):
            case (CostCategory::Operation):
            case (CostCategory::Replacement):
            case (CostCategory::MinorOverhaul):
            case (CostCategory::MajorOverhaul):
            case (CostCategory::OtherOperational):
            case (CostCategory::Construction):
            case (CostCategory::Salvage):
            case (CostCategory::OtherCapital):
            case (CostCategory::Water):
            case (CostCategory::Energy): {
                tableBody(1, jObj + 1) = CostCategoryNames[static_cast<int>(elcc->CashFlow[offset + jObj].Category)];
                break;
            }
            default:
                tableBody(1, jObj + 1) = "-";
                break;
            }
            switch (elcc->CashFlow[offset + jObj].SourceKind) {
            case (SourceKindType::Nonrecurring):
            case (SourceKindType::Recurring): {
                tableBody(2, jObj + 1) = SourceKindTypeNames[static_cast<int>(elcc->CashFlow[offset + jObj].SourceKind)];
                break;
            }
            case (SourceKindType::Resource): {
                if (elcc->CashFlow[offset + jObj].Category == CostCategory::Water) {
                    tableBody(2, jObj + 1) = SourceKindTypeNames[static_cast<int>(ResourceCostCategory::Water)];
                } else {
                    tableBody(2, jObj + 1) = SourceKindTypeNames[static_cast<int>(ResourceCostCategory::Energy)];
                }
                break;
            }
            default: {
                tableBody(2, jObj + 1) = "-";
                break;
            }
            }
            tableBody(3, jObj + 1) = RealToStr(elcc->CashFlow[offset + jObj].orginalCost, 2);
            tableBody(4, jObj + 1) = RealToStr(elcc->CashFlow[offset + jObj].presentValue, 2);
            totalPV += elcc->CashFlow[offset + jObj].presentValue;
            if (elcc->CashFlow[offset + jObj].orginalCost != 0.0) {
                tableBody(5, jObj + 1) = RealToStr(elcc->CashFlow[offset + jObj].presentValue / elcc->CashFlow[offset + jObj].orginalCost, 4);
            } else {
                tableBody(5, jObj + 1) = "-";
            }
        }
        tableBody(4, numRows + 1) = RealToStr(totalPV, 2);
        WriteSubtitle(state, "Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody,
                rowHead,
                columnHead,
                "Life-Cycle Cost Report",
                "Entire Facility",
                "Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody,
                rowHead,
                columnHead,
                "Life-Cycle Cost Report",
                "Entire Facility",
                "Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- Present Value by Category
        rowHead.allocate(16);
        columnHead.allocate(1);
        columnWidth.allocate(1);
        columnWidth = 14; // array assignment - same for all columns
        tableBody.allocate(1, 16);
        tableBody = "";
        rowHead(1) = "Construction";
        rowHead(2) = "Salvage";
        rowHead(3) = "Other Capital";
        rowHead(4) = "Energy";
        rowHead(5) = "Water";
        rowHead(6) = "Maintenance";
        rowHead(7) = "Repair";
        rowHead(8) = "Operation";
        rowHead(9) = "Replacement";
        rowHead(10) = "Minor Overhaul";
        rowHead(11) = "Major Overhaul";
        rowHead(12) = "Other Operational";
        rowHead(13) = "Total Energy";
        rowHead(14) = "Total Operation";
        rowHead(15) = "Total Capital";
        rowHead(16) = "Grand Total";
        columnHead(1) = "Present Value";

        tableBody(1, 1) = RealToStr(elcc->CashFlow[CostCategory::Construction].presentValue, 2);
        tableBody(1, 2) = RealToStr(elcc->CashFlow[CostCategory::Salvage].presentValue, 2);
        tableBody(1, 3) = RealToStr(elcc->CashFlow[CostCategory::OtherCapital].presentValue, 2);
        tableBody(1, 4) = RealToStr(elcc->CashFlow[CostCategory::Energy].presentValue, 2);
        tableBody(1, 5) = RealToStr(elcc->CashFlow[CostCategory::Water].presentValue, 2);
        tableBody(1, 6) = RealToStr(elcc->CashFlow[CostCategory::Maintenance].presentValue, 2);
        tableBody(1, 7) = RealToStr(elcc->CashFlow[CostCategory::Repair].presentValue, 2);
        tableBody(1, 8) = RealToStr(elcc->CashFlow[CostCategory::Operation].presentValue, 2);
        tableBody(1, 9) = RealToStr(elcc->CashFlow[CostCategory::Replacement].presentValue, 2);
        tableBody(1, 10) = RealToStr(elcc->CashFlow[CostCategory::MinorOverhaul].presentValue, 2);
        tableBody(1, 11) = RealToStr(elcc->CashFlow[CostCategory::MajorOverhaul].presentValue, 2);
        tableBody(1, 12) = RealToStr(elcc->CashFlow[CostCategory::OtherOperational].presentValue, 2);
        tableBody(1, 13) = RealToStr(elcc->CashFlow[CostCategory::TotEnergy].presentValue, 2);
        tableBody(1, 14) = RealToStr(elcc->CashFlow[CostCategory::TotOper].presentValue, 2);
        tableBody(1, 15) = RealToStr(elcc->CashFlow[CostCategory::TotCaptl].presentValue, 2);
        tableBody(1, 16) = RealToStr(elcc->CashFlow[CostCategory::TotGrand].presentValue, 2);

        WriteSubtitle(state, "Present Value by Category");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Category");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Category");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- Present Value by Year
        rowHead.allocate(elcc->lengthStudyYears + 1);
        columnHead.allocate(3);
        columnWidth.allocate(3);
        columnWidth = 14; // array assignment - same for all columns
        tableBody.allocate(3, elcc->lengthStudyYears + 1);
        tableBody = "";
        columnHead(1) = "Total Cost (Without Escalation)";
        columnHead(2) = "Total Cost (With Escalation)";
        columnHead(3) = "Present Value of Costs";

        totalPV = 0.0;
        for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
            rowHead(iYear) = format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->baseDateMonth)], elcc->baseDateYear + iYear - 1);
            tableBody(1, iYear) = RealToStr(elcc->CashFlow[CostCategory::TotGrand].yrAmount(iYear), 2);
            // adjust for escalated energy costs
            Real64 yearly_total_cost = elcc->CashFlow[CostCategory::TotGrand].yrAmount(iYear) + elcc->EscalatedTotEnergy(iYear) -
                                       elcc->CashFlow[CostCategory::TotEnergy].yrAmount(iYear);
            tableBody(2, iYear) = RealToStr(yearly_total_cost, 2);
            tableBody(3, iYear) = RealToStr(elcc->CashFlow[CostCategory::TotGrand].yrPresVal(iYear), 2);
            totalPV += elcc->CashFlow[CostCategory::TotGrand].yrPresVal(iYear);
        }

        rowHead(elcc->lengthStudyYears + 1) = "TOTAL";
        tableBody(3, elcc->lengthStudyYears + 1) = RealToStr(totalPV, 2);

        WriteSubtitle(state, "Present Value by Year");
        WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Year");
        }
        if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
            state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Year");
        }
        columnHead.deallocate();
        rowHead.deallocate();
        columnWidth.deallocate();
        tableBody.deallocate();
        //---- After Tax Estimate
        if (elcc->taxRate != 0.0) {
            rowHead.allocate(elcc->lengthStudyYears + 1);
            columnHead.allocate(5);
            columnWidth.allocate(5);
            columnWidth = 14; // array assignment - same for all columns
            tableBody.allocate(5, elcc->lengthStudyYears + 1);
            tableBody = "";
            columnHead(1) = "Depreciated Capital";
            columnHead(2) = "Taxable Income";
            columnHead(3) = "Income Taxes";
            columnHead(4) = "After Tax Cash Flow";
            columnHead(5) = "After Tax Present Value";

            totalPV = 0.0;
            for (iYear = 1; iYear <= elcc->lengthStudyYears; ++iYear) {
                rowHead(iYear) =
                    format("{} {}", UtilityRoutines::MonthNamesCC[static_cast<int>(elcc->baseDateMonth)], elcc->baseDateYear + iYear - 1);
                tableBody(1, iYear) = RealToStr(elcc->DepreciatedCapital(iYear), 2);
                tableBody(2, iYear) = RealToStr(elcc->TaxableIncome(iYear), 2);
                tableBody(3, iYear) = RealToStr(elcc->Taxes(iYear), 2);
                tableBody(4, iYear) = RealToStr(elcc->AfterTaxCashFlow(iYear), 2);
                tableBody(5, iYear) = RealToStr(elcc->AfterTaxPresentValue(iYear), 2);
                totalPV += elcc->AfterTaxPresentValue(iYear);
            }

            rowHead(elcc->lengthStudyYears + 1) = "TOTAL";
            tableBody(5, elcc->lengthStudyYears + 1) = RealToStr(totalPV, 2);

            WriteSubtitle(state, "After Tax Estimate");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->createSQLiteTabularDataRecords(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "After Tax Estimate");
            }
            if (state.dataResultsFramework->resultsFramework->timeSeriesAndTabularEnabled()) {
                state.dataResultsFramework->resultsFramework->TabularReportsCollection.addReportTable(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "After Tax Estimate");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
        }
    }
}

} // namespace EnergyPlus::EconomicLifeCycleCost
