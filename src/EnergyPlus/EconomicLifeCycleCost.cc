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
    using namespace DataIPShortCuts;

    int const skRecurring(1);
    int const skNonrecurring(2);
    int const skResource(3);
    int const skSum(4);

    int const pvkEnergy(1);
    int const pvkNonEnergy(2);
    int const pvkNotComputed(3);

    // present value factors
    Array1D<Real64> SPV;
    std::map<int, std::map<DataGlobalConstants::ResourceType, Real64>>  energySPV; // yearly equivalent to FEMP UPV* values

    // arrays related to computing after tax cashflow and present value
    Array1D<Real64> DepreciatedCapital;
    Array1D<Real64> TaxableIncome;
    Array1D<Real64> Taxes;
    Array1D<Real64> AfterTaxCashFlow;
    Array1D<Real64> AfterTaxPresentValue;

    // arrays related to escalated energy costs
    Array1D<Real64> EscalatedTotEnergy;
    std::map<int, std::map<DataGlobalConstants::ResourceType, Real64>> EscalatedEnergy;

    // SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

    // Object Data
    Array1D<RecurringCostsType> RecurringCosts;
    Array1D<NonrecurringCostType> NonrecurringCost;
    Array1D<UsePriceEscalationType> UsePriceEscalation;
    Array1D<UseAdjustmentType> UseAdjustment;
    Array1D<CashFlowType> CashFlow;

    namespace {
        // These were static variables within different functions. They were pulled out into the namespace
        // to facilitate easier unit testing of those functions.
        // These are purposefully not in the header file as an extern variable. No one outside of this should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool GetInput_GetLifeCycleCostInput(true);

        // from former statics in GetInputLifeCycleCostUsePriceEscalation()
        int UsePriceEscalation_escStartYear(0);
        int UsePriceEscalation_escNumYears(0);
        int UsePriceEscalation_escEndYear(0);
        int UsePriceEscalation_earlierEndYear(0);
        int UsePriceEscalation_laterStartYear(0);
        int UsePriceEscalation_curEsc(0);
        int UsePriceEscalation_curFld(0);

        // from former statics in ExpressAsCashFlows
        int ExpressAsCashFlows_baseMonths1900(0);    // number of months since 1900 for base period
        int ExpressAsCashFlows_serviceMonths1900(0); // number of months since 1900 for service period

    } // namespace

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

        // METHODOLOGY EMPLOYED:

        // REFERENCES:
        // na

        // Using/Aliasing
        using OutputReportTabular::AddTOCEntry;

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

        if (GetInput_GetLifeCycleCostInput) {
            GetInputLifeCycleCostParameters(state);
            GetInputLifeCycleCostRecurringCosts(state);
            GetInputLifeCycleCostNonrecurringCost(state);
            GetInputLifeCycleCostUsePriceEscalation(state);
            GetInputLifeCycleCostUseAdjustment(state);
            GetInput_GetLifeCycleCostInput = false;
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
        inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
        NumArray.allocate(NumNums);
        AlphaArray.allocate(NumAlphas);
        NumObj = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        if (NumObj == 0) {
            state.dataEconLifeCycleCost->LCCparamPresent = false;
        } else if (NumObj == 1) {
            state.dataEconLifeCycleCost->LCCparamPresent = true;
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          1,
                                          AlphaArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // check to make sure none of the values are another life cycle cost object
            for (jFld = 1; jFld <= NumAlphas; ++jFld) {
                if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                     " a field was found containing LifeCycleCost: which may indicate a missing comma.");
                }
            }
            // start to extract values from input array into appropriate fields
            //  A1,  \field Name
            //       \required-field
            //       \type alpha
            state.dataEconLifeCycleCost->LCCname = AlphaArray(1);
            //  A2, \field Discounting Convention
            //      \type choice
            //      \key EndOfYear
            //      \key MidYear
            //      \key BeginningOfYear
            //      \default EndOfYear
            if (UtilityRoutines::SameString(AlphaArray(2), "EndOfYear")) {
                state.dataEconLifeCycleCost->discountConvention = iDiscConv::EndOfYear;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "MidYear")) {
                state.dataEconLifeCycleCost->discountConvention = iDiscConv::MidYear;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "BeginningOfYear")) {
                state.dataEconLifeCycleCost->discountConvention = iDiscConv::BeginOfYear;
            } else {
                state.dataEconLifeCycleCost->discountConvention = iDiscConv::EndOfYear;
                ShowWarningError(state, CurrentModuleObject + ": Invalid " + cAlphaFieldNames(2) + "=\"" + AlphaArray(2) + "\". EndOfYear will be used.");
            }
            // A3,  \field Inflation Approach
            //      \type choice
            //      \key ConstantDollar
            //      \key CurrentDollar
            //      \default ConstantDollar
            if (UtilityRoutines::SameString(AlphaArray(3), "ConstantDollar")) {
                state.dataEconLifeCycleCost->inflationApproach = iInflAppr::ConstantDollar;
            } else if (UtilityRoutines::SameString(AlphaArray(3), "CurrentDollar")) {
                state.dataEconLifeCycleCost->inflationApproach = iInflAppr::CurrentDollar;
            } else {
                state.dataEconLifeCycleCost->inflationApproach = iInflAppr::ConstantDollar;
                ShowWarningError(state, CurrentModuleObject + ": Invalid " + cAlphaFieldNames(3) + "=\"" + AlphaArray(3) +
                                 "\". ConstantDollar will be used.");
            }
            // N1,  \field Real Discount Rate
            //      \type real
            state.dataEconLifeCycleCost->realDiscountRate = NumArray(1);
            if ((state.dataEconLifeCycleCost->inflationApproach == iInflAppr::ConstantDollar) && lNumericFieldBlanks(1)) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid for field " + cNumericFieldNames(1) +
                                 " to be blank when ConstantDollar analysis is be used.");
            }
            if ((state.dataEconLifeCycleCost->realDiscountRate > 0.30) || (state.dataEconLifeCycleCost->realDiscountRate < -0.30)) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(1) +
                                 ".  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. ");
            }
            // N2,  \field Nominal Discount Rate
            //      \type real
            state.dataEconLifeCycleCost->nominalDiscountRate = NumArray(2);
            if ((state.dataEconLifeCycleCost->inflationApproach == iInflAppr::CurrentDollar) && lNumericFieldBlanks(2)) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid for field " + cNumericFieldNames(2) +
                                 " to be blank when CurrentDollar analysis is be used.");
            }
            if ((state.dataEconLifeCycleCost->nominalDiscountRate > 0.30) || (state.dataEconLifeCycleCost->nominalDiscountRate < -0.30)) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(2) +
                                 ".  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. ");
            }
            // N3,  \field Inflation
            //      \type real
            state.dataEconLifeCycleCost->inflation = NumArray(3);
            if ((state.dataEconLifeCycleCost->inflationApproach == iInflAppr::ConstantDollar) && (!lNumericFieldBlanks(3))) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid for field " + cNumericFieldNames(3) +
                                 " contain a value when ConstantDollar analysis is be used.");
            }
            if ((state.dataEconLifeCycleCost->inflation > 0.30) || (state.dataEconLifeCycleCost->inflation < -0.30)) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(3) +
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
            state.dataEconLifeCycleCost->baseDateMonth = MonthToMonthNumber(AlphaArray(4), 1);
            // N4,  \field Base Date Year
            //      \type integer
            //      \minimum 1900
            //      \maximum 2100
            state.dataEconLifeCycleCost->baseDateYear = int(NumArray(4));
            if (state.dataEconLifeCycleCost->baseDateYear > 2100) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(4) +
                                 ".  Value greater than 2100 yet it is representing a year. ");
            }
            if (state.dataEconLifeCycleCost->baseDateYear < 1900) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(4) +
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
            state.dataEconLifeCycleCost->serviceDateMonth = MonthToMonthNumber(AlphaArray(5), 1);
            // N5,  \field Service Date Year
            //      \type integer
            //      \minimum 1900
            //      \maximum 2100
            state.dataEconLifeCycleCost->serviceDateYear = int(NumArray(5));
            if (state.dataEconLifeCycleCost->serviceDateYear > 2100) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(5) +
                                 ".  Value greater than 2100 yet it is representing a year. ");
            }
            if (state.dataEconLifeCycleCost->serviceDateYear < 1900) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(5) +
                                 ".  Value less than 1900 yet it is representing a year. ");
            }
            // N6,  \field Length of Study Period in Years
            //      \type integer
            //      \minimum 1
            //      \maximum 100
            state.dataEconLifeCycleCost->lengthStudyYears = int(NumArray(6));
            if (state.dataEconLifeCycleCost->lengthStudyYears > 100) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(6) +
                                 ".  A value greater than 100 is not reasonable for an economic evaluation. ");
            }
            if (state.dataEconLifeCycleCost->lengthStudyYears < 1) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(6) +
                                 ".  A value less than 1 is not reasonable for an economic evaluation. ");
            }
            state.dataEconLifeCycleCost->lengthStudyTotalMonths = state.dataEconLifeCycleCost->lengthStudyYears * 12;
            // N7, \field Tax rate
            //      \type real
            //      \minimum 0.0
            state.dataEconLifeCycleCost->taxRate = NumArray(7);
            if (state.dataEconLifeCycleCost->taxRate < 0.0 && (!lNumericFieldBlanks(7))) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(10) +
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
            if (UtilityRoutines::SameString(AlphaArray(6), "ModifiedAcceleratedCostRecoverySystem-3year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::MACRS3;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "ModifiedAcceleratedCostRecoverySystem-5year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::MACRS5;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "ModifiedAcceleratedCostRecoverySystem-7year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::MACRS7;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "ModifiedAcceleratedCostRecoverySystem-10year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::MACRS10;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "ModifiedAcceleratedCostRecoverySystem-15year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::MACRS15;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "ModifiedAcceleratedCostRecoverySystem-20year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::MACRS20;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "StraightLine-27year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::Straight27;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "StraightLine-31year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::Straight31;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "StraightLine-39year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::Straight39;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "StraightLine-40year")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::Straight40;
            } else if (UtilityRoutines::SameString(AlphaArray(6), "None")) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::None;
            } else if (lAlphaFieldBlanks(6)) {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::None;
                ShowWarningError(state, CurrentModuleObject + ": The input field " + cAlphaFieldNames(6) + "is blank. \"None\" will be used.");
            } else {
                state.dataEconLifeCycleCost->depreciationMethod = iDeprMethod::None;
                ShowWarningError(state, CurrentModuleObject + ": Invalid " + cAlphaFieldNames(6) + "=\"" + AlphaArray(6) + R"(". "None" will be used.)");
            }
            // compute derived variables
            state.dataEconLifeCycleCost->lastDateMonth = state.dataEconLifeCycleCost->baseDateMonth - 1; // same month of the year for first and last month
            if (state.dataEconLifeCycleCost->lastDateMonth == 0) state.dataEconLifeCycleCost->lastDateMonth = 12;
            state.dataEconLifeCycleCost->lastDateYear = state.dataEconLifeCycleCost->baseDateYear + state.dataEconLifeCycleCost->lengthStudyYears - 1;
        } else {
            ShowWarningError(state, CurrentModuleObject + ": Only one instance of this object is allowed. No life-cycle cost reports will be generated.");
            state.dataEconLifeCycleCost->LCCparamPresent = false;
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

        if (!state.dataEconLifeCycleCost->LCCparamPresent) return;
        CurrentModuleObject = "LifeCycleCost:RecurringCosts";
        inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
        NumArray.allocate(NumNums);
        AlphaArray.allocate(NumAlphas);
        state.dataEconLifeCycleCost->numRecurringCosts = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        RecurringCosts.allocate(state.dataEconLifeCycleCost->numRecurringCosts);
        for (iInObj = 1; iInObj <= state.dataEconLifeCycleCost->numRecurringCosts; ++iInObj) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          iInObj,
                                          AlphaArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // check to make sure none of the values are another life cycle cost object
            for (jFld = 1; jFld <= NumAlphas; ++jFld) {
                if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                     " a field was found containing LifeCycleCost: which may indicate a missing comma.");
                }
            }
            // start to extract values from input array into appropriate fields
            //   A1,  \field Name
            //        \required-field
            //        \type alpha
            RecurringCosts(iInObj).name = AlphaArray(1);
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
            if (UtilityRoutines::SameString(AlphaArray(2), "Maintenance")) {
                RecurringCosts(iInObj).category = costCatMaintenance;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "Repair")) {
                RecurringCosts(iInObj).category = costCatRepair;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "Operation")) {
                RecurringCosts(iInObj).category = costCatOperation;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "Replacement")) {
                RecurringCosts(iInObj).category = costCatReplacement;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "MinorOverhaul")) {
                RecurringCosts(iInObj).category = costCatMinorOverhaul;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "MajorOverhaul")) {
                RecurringCosts(iInObj).category = costCatMajorOverhaul;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "OtherOperational")) {
                RecurringCosts(iInObj).category = costCatOtherOperational;
            } else {
                RecurringCosts(iInObj).category = costCatMaintenance;
                ShowWarningError(state, CurrentModuleObject + ": Invalid " + cAlphaFieldNames(2) + "=\"" + AlphaArray(2) +
                                 "\". The category of Maintenance will be used.");
            }
            //   N1,  \field Cost
            //        \type real
            RecurringCosts(iInObj).cost = NumArray(1);
            //   A3,  \field Start of Costs
            //        \type choice
            //        \key ServicePeriod
            //        \key BasePeriod
            //        \default ServicePeriod
            if (UtilityRoutines::SameString(AlphaArray(3), "ServicePeriod")) {
                RecurringCosts(iInObj).startOfCosts = iStartCosts::ServicePeriod;
            } else if (UtilityRoutines::SameString(AlphaArray(3), "BasePeriod")) {
                RecurringCosts(iInObj).startOfCosts = iStartCosts::BasePeriod;
            } else {
                RecurringCosts(iInObj).startOfCosts = iStartCosts::ServicePeriod;
                ShowWarningError(state, CurrentModuleObject + ": Invalid " + cAlphaFieldNames(3) + "=\"" + AlphaArray(3) +
                                 "\". The start of the service period will be used.");
            }
            //   N2,  \field Years from Start
            //        \type integer
            //        \minimum 0
            //        \maximum 100
            RecurringCosts(iInObj).yearsFromStart = int(NumArray(2));
            if (RecurringCosts(iInObj).yearsFromStart > 100) {
                ShowWarningError(state,
                    CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(2) +
                    ".  This value is the number of years from the start so a value greater than 100 is not reasonable for an economic evaluation. ");
            }
            if (RecurringCosts(iInObj).yearsFromStart < 0) {
                ShowWarningError(state,
                    CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(2) +
                    ".  This value is the number of years from the start so a value less than 0 is not reasonable for an economic evaluation. ");
            }
            //   N3,  \field Months from Start
            //        \type integer
            //        \minimum 0
            //        \maximum 1200
            RecurringCosts(iInObj).monthsFromStart = int(NumArray(3));
            if (RecurringCosts(iInObj).monthsFromStart > 1200) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(3) +
                                 ".  This value is the number of months from the start so a value greater than 1200 is not reasonable for an "
                                 "economic evaluation. ");
            }
            if (RecurringCosts(iInObj).monthsFromStart < 0) {
                ShowWarningError(state,
                    CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(3) +
                    ".  This value is the number of months from the start so a value less than 0 is not reasonable for an economic evaluation. ");
            }
            //   N4,  \field Repeat Period Years
            //        \type integer
            //        \minimum 1
            //        \maximum 100
            RecurringCosts(iInObj).repeatPeriodYears = int(NumArray(4));
            if (RecurringCosts(iInObj).repeatPeriodYears > 100) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(4) +
                                 ".  This value is the number of years between occurrences of the cost so a value greater than 100 is not reasonable "
                                 "for an economic evaluation. ");
            }
            if (RecurringCosts(iInObj).repeatPeriodYears < 1) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(4) +
                                 ".  This value is the number of years between occurrences of the cost so a value less than 1 is not reasonable for "
                                 "an economic evaluation. ");
            }
            //   N5,  \field Repeat Period Months
            //        \type integer
            //        \minimum 0
            //        \maximum 1200
            RecurringCosts(iInObj).repeatPeriodMonths = int(NumArray(5));
            if (RecurringCosts(iInObj).repeatPeriodMonths > 1200) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(5) +
                                 ".  This value is the number of months between occurrences of the cost so a value greater than 1200 is not "
                                 "reasonable for an economic evaluation. ");
            }
            if (RecurringCosts(iInObj).repeatPeriodMonths < 0) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(5) +
                                 ".  This value is the number of months between occurrences of the cost so a value less than 0 is not reasonable for "
                                 "an economic evaluation. ");
            }
            if ((RecurringCosts(iInObj).repeatPeriodMonths == 0) && (RecurringCosts(iInObj).repeatPeriodYears == 0)) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in fields " + cNumericFieldNames(5) + " and " + cNumericFieldNames(4) +
                                 ".  The repeat period must not be zero months and zero years. ");
            }
            //   N6;  \field Annual escalation rate
            //        \type real
            RecurringCosts(iInObj).annualEscalationRate = int(NumArray(6));
            if (RecurringCosts(iInObj).annualEscalationRate > 0.30) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(6) +
                                 ".  This value is the decimal value for the annual escalation so most values are between 0.02 and 0.15. ");
            }
            if (RecurringCosts(iInObj).annualEscalationRate < -0.30) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(6) +
                                 ".  This value is the decimal value for the annual escalation so most values are between 0.02 and 0.15. ");
            }
            // express the years and months fields in total months
            RecurringCosts(iInObj).totalMonthsFromStart = RecurringCosts(iInObj).yearsFromStart * 12 + RecurringCosts(iInObj).monthsFromStart;
            RecurringCosts(iInObj).totalRepeatPeriodMonths =
                RecurringCosts(iInObj).repeatPeriodYears * 12 + RecurringCosts(iInObj).repeatPeriodMonths;
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

        if (!state.dataEconLifeCycleCost->LCCparamPresent) return;
        CurrentModuleObject = "LifeCycleCost:NonrecurringCost";
        inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
        NumArray.allocate(NumNums);
        AlphaArray.allocate(NumAlphas);
        state.dataEconLifeCycleCost->numNonrecurringCost = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        numComponentCostLineItems = inputProcessor->getNumObjectsFound(state, "ComponentCost:LineItem");
        if (numComponentCostLineItems > 0) {                    // leave room for component cost total
            NonrecurringCost.allocate(state.dataEconLifeCycleCost->numNonrecurringCost + 1); // add a place for CostEstimate total
        } else {
            NonrecurringCost.allocate(state.dataEconLifeCycleCost->numNonrecurringCost);
        }
        for (iInObj = 1; iInObj <= state.dataEconLifeCycleCost->numNonrecurringCost; ++iInObj) {
            inputProcessor->getObjectItem(state,
                                          CurrentModuleObject,
                                          iInObj,
                                          AlphaArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // check to make sure none of the values are another life cycle cost object
            for (jFld = 1; jFld <= NumAlphas; ++jFld) {
                if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                     " a field was found containing LifeCycleCost: which may indicate a missing comma.");
                }
            }
            // start to extract values from input array into appropriate fields
            // A1,  \field Name
            //      \required-field
            //      \type alpha
            NonrecurringCost(iInObj).name = AlphaArray(1);
            // A2,  \field Category
            //      \type choice
            //      \key Construction
            //      \key Salvage
            //      \key OtherCapital
            //      \default Construction
            if (UtilityRoutines::SameString(AlphaArray(2), "Construction")) {
                NonrecurringCost(iInObj).category = costCatConstruction;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "Salvage")) {
                NonrecurringCost(iInObj).category = costCatSalvage;
            } else if (UtilityRoutines::SameString(AlphaArray(2), "OtherCapital")) {
                NonrecurringCost(iInObj).category = costCatOtherCapital;
            } else {
                NonrecurringCost(iInObj).category = costCatConstruction;
                ShowWarningError(state, CurrentModuleObject + ": Invalid " + cAlphaFieldNames(2) + "=\"" + AlphaArray(2) +
                                 "\". The category of Construction will be used.");
            }
            // N1,  \field Cost
            //      \type real
            NonrecurringCost(iInObj).cost = NumArray(1);
            // A3,  \field Start of Costs
            //      \type choice
            //      \key ServicePeriod
            //      \key BasePeriod
            //      \default ServicePeriod
            if (UtilityRoutines::SameString(AlphaArray(3), "ServicePeriod")) {
                NonrecurringCost(iInObj).startOfCosts = iStartCosts::ServicePeriod;
            } else if (UtilityRoutines::SameString(AlphaArray(3), "BasePeriod")) {
                NonrecurringCost(iInObj).startOfCosts = iStartCosts::BasePeriod;
            } else {
                NonrecurringCost(iInObj).startOfCosts = iStartCosts::ServicePeriod;
                ShowWarningError(state, CurrentModuleObject + ": Invalid " + cAlphaFieldNames(3) + "=\"" + AlphaArray(3) +
                                 "\". The start of the service period will be used.");
            }
            // N2,  \field Years from Start
            //      \type integer
            //      \minimum 0
            //      \maximum 100
            NonrecurringCost(iInObj).yearsFromStart = int(NumArray(2));
            if (NonrecurringCost(iInObj).yearsFromStart > 100) {
                ShowWarningError(state,
                    CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(2) +
                    ".  This value is the number of years from the start so a value greater than 100 is not reasonable for an economic evaluation. ");
            }
            if (NonrecurringCost(iInObj).yearsFromStart < 0) {
                ShowWarningError(state,
                    CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(2) +
                    ".  This value is the number of years from the start so a value less than 0 is not reasonable for an economic evaluation. ");
            }
            //  N3;  \field Months from Start
            //       \type integer
            //       \minimum 0
            //       \maximum 11
            NonrecurringCost(iInObj).monthsFromStart = int(NumArray(3));
            if (NonrecurringCost(iInObj).monthsFromStart > 1200) {
                ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(3) +
                                 ".  This value is the number of months from the start so a value greater than 1200 is not reasonable for an "
                                 "economic evaluation. ");
            }
            if (NonrecurringCost(iInObj).monthsFromStart < 0) {
                ShowWarningError(state,
                    CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(3) +
                    ".  This value is the number of months from the start so a value less than 0 is not reasonable for an economic evaluation. ");
            }
            // express the years and months fields in total months
            NonrecurringCost(iInObj).totalMonthsFromStart = NonrecurringCost(iInObj).yearsFromStart * 12 + NonrecurringCost(iInObj).monthsFromStart;
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

        if (!state.dataEconLifeCycleCost->LCCparamPresent) return;
        CurrentModuleObject = "LifeCycleCost:UsePriceEscalation";
        inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
        NumArray.allocate(NumNums);
        AlphaArray.allocate(NumAlphas);
        state.dataEconLifeCycleCost->numUsePriceEscalation = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        UsePriceEscalation.allocate(state.dataEconLifeCycleCost->numUsePriceEscalation);
        for (iInObj = 1; iInObj <= state.dataEconLifeCycleCost->numUsePriceEscalation; ++iInObj) {
            UsePriceEscalation(iInObj).Escalation.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
        }
        if (state.dataEconLifeCycleCost->numUsePriceEscalation > 0) {
            for (iInObj = 1; iInObj <= state.dataEconLifeCycleCost->numUsePriceEscalation; ++iInObj) {
                inputProcessor->getObjectItem(state,
                                              CurrentModuleObject,
                                              iInObj,
                                              AlphaArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                // check to make sure none of the values are another life cycle cost object
                for (jFld = 1; jFld <= NumAlphas; ++jFld) {
                    if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                        ShowWarningError(state, "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                         " a field was found containing LifeCycleCost: which may indicate a missing comma.");
                    }
                }
                // start to extract values from input array into appropriate fields
                // A1,  \field Name
                //      \required-field
                //      \type alpha
                UsePriceEscalation(iInObj).name = AlphaArray(1);
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
                UsePriceEscalation(iInObj).resource = AssignResourceTypeNum(AlphaArray(2)); // use function from DataGlobalConstants
                if (NumAlphas > 3) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " contains more alpha fields than expected.");
                }
                // N1,  \field Escalation Start Year
                //      \type integer
                //      \minimum 1900
                //      \maximum 2100
                UsePriceEscalation(iInObj).escalationStartYear = int(NumArray(1));
                if (UsePriceEscalation(iInObj).escalationStartYear > 2100) {
                    ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(1) +
                                     ".  Value greater than 2100 yet it is representing a year. ");
                }
                if (UsePriceEscalation(iInObj).escalationStartYear < 1900) {
                    ShowWarningError(state, CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames(1) +
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
                UsePriceEscalation(iInObj).escalationStartMonth = MonthToMonthNumber(AlphaArray(3), 1);
                // N2,  \field Year 1 Escalation
                //      \type real
                //      \begin-extensible
                // The array is from the baseDateYear until baseDateYear + lengthStudyYears
                // Set the array to default to 1.0
                for (jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
                    UsePriceEscalation(iInObj).Escalation(jYear) = 1.0;
                }
                // Since the years in the UsePriceEscalation may not match up with the baseDateYear and
                // the lenghtStudyYears, need to make adjustments when reading in the values to align
                // with the baseDateYear (the first item in all yearly arrays)
                UsePriceEscalation_escStartYear = UsePriceEscalation(iInObj).escalationStartYear;
                UsePriceEscalation_escNumYears = NumNums - 1;
                UsePriceEscalation_escEndYear = UsePriceEscalation_escStartYear + UsePriceEscalation_escNumYears - 1;
                UsePriceEscalation_earlierEndYear = min(UsePriceEscalation_escEndYear, state.dataEconLifeCycleCost->lastDateYear);   // pick the earlier ending date
                UsePriceEscalation_laterStartYear = max(UsePriceEscalation_escStartYear, state.dataEconLifeCycleCost->baseDateYear); // pick the later starting date
                for (jYear = UsePriceEscalation_laterStartYear; jYear <= UsePriceEscalation_earlierEndYear; ++jYear) {
                    UsePriceEscalation_curFld = 2 + jYear - UsePriceEscalation_escStartYear;
                    UsePriceEscalation_curEsc = 1 + jYear - state.dataEconLifeCycleCost->baseDateYear;
                    if ((UsePriceEscalation_curFld <= NumNums) && (UsePriceEscalation_curFld >= 1)) {
                        if ((UsePriceEscalation_curEsc <= state.dataEconLifeCycleCost->lengthStudyYears) && (UsePriceEscalation_curEsc >= 1)) {
                            UsePriceEscalation(iInObj).Escalation(UsePriceEscalation_curEsc) = NumArray(UsePriceEscalation_curFld);
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

        if (!state.dataEconLifeCycleCost->LCCparamPresent) return;
        CurrentModuleObject = "LifeCycleCost:UseAdjustment";
        inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNums);
        NumArray.allocate(NumNums);
        AlphaArray.allocate(NumAlphas);
        state.dataEconLifeCycleCost->numUseAdjustment = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        UseAdjustment.allocate(state.dataEconLifeCycleCost->numUseAdjustment);
        for (iInObj = 1; iInObj <= state.dataEconLifeCycleCost->numUseAdjustment; ++iInObj) {
            UseAdjustment(iInObj).Adjustment.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
        }
        if (state.dataEconLifeCycleCost->numUseAdjustment > 0) {
            for (iInObj = 1; iInObj <= state.dataEconLifeCycleCost->numUseAdjustment; ++iInObj) {
                inputProcessor->getObjectItem(state,
                                              CurrentModuleObject,
                                              iInObj,
                                              AlphaArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                // check to make sure none of the values are another life cycle cost object
                for (jFld = 1; jFld <= NumAlphas; ++jFld) {
                    if (hasi(AlphaArray(jFld), "LifeCycleCost:")) {
                        ShowWarningError(state, "In " + CurrentModuleObject + " named " + AlphaArray(1) +
                                         " a field was found containing LifeCycleCost: which may indicate a missing comma.");
                    }
                }
                // start to extract values from input array into appropriate fields
                //  A1,  \field Name
                //       \required-field
                //       \type alpha
                UseAdjustment(iInObj).name = AlphaArray(1);
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
                UseAdjustment(iInObj).resource = AssignResourceTypeNum(AlphaArray(2)); // use function from DataGlobalConstants
                if (NumAlphas > 2) {
                    ShowWarningError(state, "In " + CurrentModuleObject + " contains more alpha fields than expected.");
                }
                //  N1,  \field Year 1 Multiplier
                //       \type real
                //       \begin-extensible
                // Set the array to default to 1.0
                for (jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
                    UseAdjustment(iInObj).Adjustment(jYear) = 1.0;
                }
                numFldsToUse = min(NumNums, state.dataEconLifeCycleCost->lengthStudyYears);
                for (jYear = 1; jYear <= numFldsToUse; ++jYear) {
                    UseAdjustment(iInObj).Adjustment(jYear) = NumArray(jYear);
                }
            }
        }
    }

    int MonthToMonthNumber(std::string const &inMonthString, int const inDefaultMonth)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   May 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        //  Convert the string of the name of the month into numbers 1 to 12

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        int MonthToMonthNumber;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        if (UtilityRoutines::SameString(inMonthString, "January")) {
            MonthToMonthNumber = 1;
        } else if (UtilityRoutines::SameString(inMonthString, "February")) {
            MonthToMonthNumber = 2;
        } else if (UtilityRoutines::SameString(inMonthString, "March")) {
            MonthToMonthNumber = 3;
        } else if (UtilityRoutines::SameString(inMonthString, "April")) {
            MonthToMonthNumber = 4;
        } else if (UtilityRoutines::SameString(inMonthString, "May")) {
            MonthToMonthNumber = 5;
        } else if (UtilityRoutines::SameString(inMonthString, "June")) {
            MonthToMonthNumber = 6;
        } else if (UtilityRoutines::SameString(inMonthString, "July")) {
            MonthToMonthNumber = 7;
        } else if (UtilityRoutines::SameString(inMonthString, "August")) {
            MonthToMonthNumber = 8;
        } else if (UtilityRoutines::SameString(inMonthString, "September")) {
            MonthToMonthNumber = 9;
        } else if (UtilityRoutines::SameString(inMonthString, "October")) {
            MonthToMonthNumber = 10;
        } else if (UtilityRoutines::SameString(inMonthString, "November")) {
            MonthToMonthNumber = 11;
        } else if (UtilityRoutines::SameString(inMonthString, "December")) {
            MonthToMonthNumber = 12;
        } else {
            MonthToMonthNumber = inDefaultMonth;
        }
        return MonthToMonthNumber;
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
            for (auto iResource : DataGlobalConstants::AllResourceTypes) {
                monthMap.insert(std::pair<DataGlobalConstants::ResourceType, Real64> (iResource, 0.0));
            }
            resourceCosts.insert(std::pair<int, std::map<DataGlobalConstants::ResourceType, Real64>> (jMonth, monthMap));
        }

        Array1D<Real64> curResourceCosts(12);

        std::map<DataGlobalConstants::ResourceType, bool> resourceCostNotZero;
        for (auto iResource : DataGlobalConstants::AllResourceTypes) {
            resourceCostNotZero.insert(std::pair<DataGlobalConstants::ResourceType, bool>(iResource, false));
        }

        std::map<DataGlobalConstants::ResourceType, Real64> resourceCostAnnual;
        for (auto iResource : DataGlobalConstants::AllResourceTypes) {
            resourceCostAnnual.insert(std::pair<DataGlobalConstants::ResourceType, Real64>(iResource, 0.0));
        }

        Real64 annualCost;
        int cashFlowCounter;
        int found;
        int curCategory;
        Array1D<Real64> monthlyInflationFactor;
        Real64 inflationPerMonth;
        int iLoop;

        // compute months from 1900 for base and service period
        ExpressAsCashFlows_baseMonths1900 = (state.dataEconLifeCycleCost->baseDateYear - 1900) * 12 + state.dataEconLifeCycleCost->baseDateMonth;
        ExpressAsCashFlows_serviceMonths1900 = (state.dataEconLifeCycleCost->serviceDateYear - 1900) * 12 + state.dataEconLifeCycleCost->serviceDateMonth;
        monthsBaseToService = ExpressAsCashFlows_serviceMonths1900 - ExpressAsCashFlows_baseMonths1900;
        // if ComponentCost:LineItem exist, the grand total of all costs are another non-recurring cost
        if (state.dataCostEstimateManager->CurntBldg.GrandTotal > 0.0) { // from DataCostEstimate and computed in WriteCompCostTable within OutputReportTabular
            ++state.dataEconLifeCycleCost->numNonrecurringCost;
            NonrecurringCost(state.dataEconLifeCycleCost->numNonrecurringCost).name = "Total of ComponentCost:*";
            NonrecurringCost(state.dataEconLifeCycleCost->numNonrecurringCost).lineItem = "";
            NonrecurringCost(state.dataEconLifeCycleCost->numNonrecurringCost).category = costCatConstruction;
            NonrecurringCost(state.dataEconLifeCycleCost->numNonrecurringCost).cost = state.dataCostEstimateManager->CurntBldg.GrandTotal;
            NonrecurringCost(state.dataEconLifeCycleCost->numNonrecurringCost).startOfCosts = iStartCosts::BasePeriod;
            NonrecurringCost(state.dataEconLifeCycleCost->numNonrecurringCost).yearsFromStart = 0;
            NonrecurringCost(state.dataEconLifeCycleCost->numNonrecurringCost).monthsFromStart = 0;
            NonrecurringCost(state.dataEconLifeCycleCost->numNonrecurringCost).totalMonthsFromStart = 0;
        }

        // gather costs from EconomicTariff for each end use
        state.dataEconLifeCycleCost->numResourcesUsed = 0;
        for (auto iResource : DataGlobalConstants::AllResourceTypes) {
            GetMonthlyCostForResource(state, iResource, curResourceCosts);
            annualCost = 0.0;
            for (int jMonth = 1; jMonth <= 12; ++jMonth) {
                resourceCosts.at(jMonth).at(iResource) = curResourceCosts(jMonth);
                annualCost += resourceCosts.at(jMonth).at(iResource);
            }
            if (annualCost != 0.0) {
                ++state.dataEconLifeCycleCost->numResourcesUsed;
                resourceCostNotZero.at(iResource) = true;
            } else {
                resourceCostNotZero.at(iResource) = false;
            }
            resourceCostAnnual.at(iResource) = annualCost;
        }
        // allocate the escalated energy cost arrays
        for (int year = 1; year <= state.dataEconLifeCycleCost->lengthStudyYears; ++year) {
            std::map<DataGlobalConstants::ResourceType, Real64> yearMap;
            for (auto iResource : DataGlobalConstants::AllResourceTypes) {
                yearMap.insert(std::pair<DataGlobalConstants::ResourceType, Real64> (iResource, 0.0));
            }
            EscalatedEnergy.insert(std::pair<int, std::map<DataGlobalConstants::ResourceType, Real64>>(year, yearMap));
        }

        EscalatedTotEnergy.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
        EscalatedTotEnergy = 0.0;

        // pre-compute the inflation factors for each year
        monthlyInflationFactor.allocate(state.dataEconLifeCycleCost->lengthStudyTotalMonths);
        if (state.dataEconLifeCycleCost->inflationApproach == iInflAppr::ConstantDollar) {
            monthlyInflationFactor = 1.0; // not really used but just in case
        } else if (state.dataEconLifeCycleCost->inflationApproach == iInflAppr::CurrentDollar) {
            // to allocate an interest rate (in this case inflation) cannot just use 1/12
            // for the monthly value since it will be slightly wrong. Instead use inverse of
            // formula from Newnan (4-32) which is r = m x (ia + 1)^(1/m) - 1)
            inflationPerMonth = std::pow(state.dataEconLifeCycleCost->inflation + 1.0, 1.0 / 12.0) - 1;
            for (int jMonth = 1; jMonth <= state.dataEconLifeCycleCost->lengthStudyTotalMonths; ++jMonth) {
                monthlyInflationFactor(jMonth) = std::pow(1.0 + inflationPerMonth, jMonth - 1);
            }
        }

        state.dataEconLifeCycleCost->numCashFlow = countOfCostCat + state.dataEconLifeCycleCost->numRecurringCosts + state.dataEconLifeCycleCost->numNonrecurringCost + state.dataEconLifeCycleCost->numResourcesUsed;
        // Cashflow array order:
        //   1 cost categories
        //   2 recurring costs
        //   3 nonrecurring costs
        //   4 resource costs
        CashFlow.allocate(state.dataEconLifeCycleCost->numCashFlow);
        for (iCashFlow = 1; iCashFlow <= state.dataEconLifeCycleCost->numCashFlow; ++iCashFlow) {
            CashFlow(iCashFlow).mnAmount.allocate(state.dataEconLifeCycleCost->lengthStudyTotalMonths);
            CashFlow(iCashFlow).yrAmount.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
            CashFlow(iCashFlow).yrPresVal.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
            CashFlow(iCashFlow).mnAmount = 0.0;  // zero all cash flow values
            CashFlow(iCashFlow).yrAmount = 0.0;  // zero all cash flow values
            CashFlow(iCashFlow).yrPresVal = 0.0; // zero all present values
        }
        // Put nonrecurring costs into cashflows
        offset = countOfCostCat + state.dataEconLifeCycleCost->numRecurringCosts;
        for (jCost = 1; jCost <= state.dataEconLifeCycleCost->numNonrecurringCost; ++jCost) {
            CashFlow(offset + jCost).name = NonrecurringCost(jCost).name;
            CashFlow(offset + jCost).SourceKind = skNonrecurring;
            CashFlow(offset + jCost).Category = NonrecurringCost(jCost).category;
            CashFlow(offset + jCost).orginalCost = NonrecurringCost(jCost).cost;
            CashFlow(offset + jCost).mnAmount = 0.0;
            if (NonrecurringCost(jCost).startOfCosts == iStartCosts::ServicePeriod) {
                month = NonrecurringCost(jCost).totalMonthsFromStart + monthsBaseToService + 1;
            } else if (NonrecurringCost(jCost).startOfCosts == iStartCosts::BasePeriod) {
                month = NonrecurringCost(jCost).totalMonthsFromStart + 1;
            }
            if ((month >= 1) && (month <= state.dataEconLifeCycleCost->lengthStudyTotalMonths)) {
                CashFlow(offset + jCost).mnAmount(month) = NonrecurringCost(jCost).cost * monthlyInflationFactor(month);
            } else {
                ShowWarningError(state, "For life cycle costing a nonrecurring cost named " + NonrecurringCost(jCost).name +
                                 " contains a cost which is not within the study period.");
            }
        }
        // Put recurring costs into cashflows
        offset = countOfCostCat;
        for (jCost = 1; jCost <= state.dataEconLifeCycleCost->numRecurringCosts; ++jCost) {
            CashFlow(offset + jCost).name = RecurringCosts(jCost).name;
            CashFlow(offset + jCost).SourceKind = skRecurring;
            CashFlow(offset + jCost).Category = RecurringCosts(jCost).category;
            CashFlow(offset + jCost).orginalCost = RecurringCosts(jCost).cost;
            if (RecurringCosts(jCost).startOfCosts == iStartCosts::ServicePeriod) {
                firstMonth = RecurringCosts(jCost).totalMonthsFromStart + monthsBaseToService + 1;
            } else if (RecurringCosts(jCost).startOfCosts == iStartCosts::BasePeriod) {
                firstMonth = RecurringCosts(jCost).totalMonthsFromStart + 1;
            }
            if ((firstMonth >= 1) && (firstMonth <= state.dataEconLifeCycleCost->lengthStudyTotalMonths)) {
                month = firstMonth;
                if (RecurringCosts(jCost).totalRepeatPeriodMonths >= 1) {
                    for (iLoop = 1; iLoop <= 10000; ++iLoop) { // add a limit to the loop to prevent runaway condition
                        CashFlow(offset + jCost).mnAmount(month) = RecurringCosts(jCost).cost * monthlyInflationFactor(month);
                        month += RecurringCosts(jCost).totalRepeatPeriodMonths;
                        if (month > state.dataEconLifeCycleCost->lengthStudyTotalMonths) break;
                    }
                }
            } else {
                ShowWarningError(state, "For life cycle costing the recurring cost named " + RecurringCosts(jCost).name +
                                 " has the first year of the costs that is not within the study period.");
            }
        }
        // Put resource costs into cashflows
        // the first cash flow for resources should be after the categories, recurring and nonrecurring costs
        cashFlowCounter = countOfCostCat + state.dataEconLifeCycleCost->numRecurringCosts + state.dataEconLifeCycleCost->numNonrecurringCost;
        for (auto iResource : DataGlobalConstants::AllResourceTypes) {
            if (resourceCostNotZero.at(iResource)) {
                ++cashFlowCounter;

                switch(iResource) {
                    case DataGlobalConstants::ResourceType::Water:
                    case DataGlobalConstants::ResourceType::OnSiteWater:
                    case DataGlobalConstants::ResourceType::MainsWater:
                    case DataGlobalConstants::ResourceType::RainWater:
                    case DataGlobalConstants::ResourceType::WellWater:
                    case DataGlobalConstants::ResourceType::Condensate:
                        CashFlow(cashFlowCounter).Category = costCatWater;
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
                        CashFlow(cashFlowCounter).Category = costCatEnergy;
                        break;
                    default:
                        CashFlow(cashFlowCounter).Category = costCatOperation;
                }

                CashFlow(cashFlowCounter).Resource = iResource;
                CashFlow(cashFlowCounter).SourceKind = skResource;
                CashFlow(cashFlowCounter).name = GetResourceTypeChar(iResource);
                if (cashFlowCounter <= state.dataEconLifeCycleCost->numCashFlow) {
                    // put the monthly energy costs into the cashflow prior to adjustments
                    // energy costs (a.k.a. resource costs) start at the start of service and repeat
                    // until the end of the study total
                    for (int jMonth = 1; jMonth <= 12; ++jMonth) {
                        CashFlow(cashFlowCounter).mnAmount(monthsBaseToService + jMonth) = resourceCosts.at(jMonth).at(iResource);
                    }
                    CashFlow(cashFlowCounter).orginalCost = resourceCostAnnual.at(iResource);
                    for (int jMonth = monthsBaseToService + 13; jMonth <= state.dataEconLifeCycleCost->lengthStudyTotalMonths; ++jMonth) {
                        // use the cost from a year earlier
                        CashFlow(cashFlowCounter).mnAmount(jMonth) = CashFlow(cashFlowCounter).mnAmount(jMonth - 12);
                    }
                    // add in the impact of inflation
                    for (int jMonth = 1; jMonth <= state.dataEconLifeCycleCost->lengthStudyTotalMonths; ++jMonth) {
                        CashFlow(cashFlowCounter).mnAmount(jMonth) *= monthlyInflationFactor(jMonth);
                    }
                    // now factor in adjustments
                    // need to find the correct adjustment to use for the current resource
                    found = 0;
                    for (jAdj = 1; jAdj <= state.dataEconLifeCycleCost->numUseAdjustment; ++jAdj) {
                        if (UseAdjustment(jAdj).resource == iResource) {
                            found = jAdj;
                            break;
                        }
                    }
                    // if any adjustments were found for that resource apply the multiplier
                    if (found != 0) {
                        for (kYear = 1; kYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++kYear) { // if service period is later than base period then this will go too far
                            for (int jMonth = 1; jMonth <= 12; ++jMonth) {
                                month = (kYear - 1) * 12 + jMonth;
                                if (month > state.dataEconLifeCycleCost->lengthStudyTotalMonths) break;
                                CashFlow(cashFlowCounter).mnAmount(month) *= UseAdjustment(found).Adjustment(kYear);
                            }
                        }
                    }
                }
            }
        }
        // put cashflows into categories
        for (jCost = 1; jCost <= countOfCostCat; ++jCost) {
            CashFlow(jCost).Category = jCost; // make each category the type indicated
            CashFlow(jCost).SourceKind = skSum;
        }
        // add the cashflows by category
        for (jCost = countOfCostCat + 1; jCost <= state.dataEconLifeCycleCost->numCashFlow; ++jCost) {
            curCategory = CashFlow(jCost).Category;
            if ((curCategory <= countOfCostCat) && (curCategory >= 1)) {
                for (int jMonth = 1; jMonth <= state.dataEconLifeCycleCost->lengthStudyTotalMonths; ++jMonth) {
                    CashFlow(curCategory).mnAmount(jMonth) += CashFlow(jCost).mnAmount(jMonth);
                }
            }
        }
        // create total categories
        for (int jMonth = 1; jMonth <= state.dataEconLifeCycleCost->lengthStudyTotalMonths; ++jMonth) {
            CashFlow(costCatTotEnergy).mnAmount(jMonth) = CashFlow(costCatEnergy).mnAmount(jMonth);
            CashFlow(costCatTotOper).mnAmount(jMonth) = CashFlow(costCatMaintenance).mnAmount(jMonth) + CashFlow(costCatRepair).mnAmount(jMonth) +
                                                        CashFlow(costCatOperation).mnAmount(jMonth) + CashFlow(costCatReplacement).mnAmount(jMonth) +
                                                        CashFlow(costCatMinorOverhaul).mnAmount(jMonth) +
                                                        CashFlow(costCatMajorOverhaul).mnAmount(jMonth) +
                                                        CashFlow(costCatOtherOperational).mnAmount(jMonth) + CashFlow(costCatWater).mnAmount(jMonth) +
                                                        CashFlow(costCatEnergy).mnAmount(jMonth);
            CashFlow(costCatTotCaptl).mnAmount(jMonth) = CashFlow(costCatConstruction).mnAmount(jMonth) + CashFlow(costCatSalvage).mnAmount(jMonth) +
                                                         CashFlow(costCatOtherCapital).mnAmount(jMonth);
            CashFlow(costCatTotGrand).mnAmount(jMonth) = CashFlow(costCatTotOper).mnAmount(jMonth) + CashFlow(costCatTotCaptl).mnAmount(jMonth);
        }
        // convert all monthly cashflows into yearly cashflows
        for (jCost = 1; jCost <= state.dataEconLifeCycleCost->numCashFlow; ++jCost) {
            for (kYear = 1; kYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++kYear) {
                annualCost = 0.0;
                for (int jMonth = 1; jMonth <= 12; ++jMonth) {
                    month = (kYear - 1) * 12 + jMonth;
                    if (month <= state.dataEconLifeCycleCost->lengthStudyTotalMonths) {
                        annualCost += CashFlow(jCost).mnAmount(month);
                    }
                }
                CashFlow(jCost).yrAmount(kYear) = annualCost;
            }
        }
        // generate a warning if resource referenced was not used
        for (int nUsePriceEsc = 1; nUsePriceEsc <= state.dataEconLifeCycleCost->numUsePriceEscalation; ++nUsePriceEsc) {
            auto curResource = UsePriceEscalation(nUsePriceEsc).resource;
            if (!resourceCostNotZero.at(curResource) && state.dataGlobal->DoWeathSim) {
                ShowWarningError(state, "The resource referenced by LifeCycleCost:UsePriceEscalation= \"" + UsePriceEscalation(nUsePriceEsc).name +
                                 "\" has no energy cost. ");
                ShowContinueError(state, "... It is likely that the wrong resource is used. The resource should match the meter used in Utility:Tariff.");
            }
        }
    }

    void ComputeEscalatedEnergyCosts(EnergyPlusData &state)
    {
        // J. Glazer - August 2019
        int nUsePriceEsc;

         for (int iCashFlow = 1; iCashFlow <= state.dataEconLifeCycleCost->numCashFlow; ++iCashFlow) {
            if (CashFlow(iCashFlow).pvKind == pvkEnergy) {
                // make sure this is not water
                auto curResource = CashFlow(iCashFlow).Resource;
                if (CashFlow(iCashFlow).Resource == DataGlobalConstants::ResourceType::Water ||
                    (CashFlow(iCashFlow).Resource >= DataGlobalConstants::ResourceType::OnSiteWater && CashFlow(iCashFlow).Resource <= DataGlobalConstants::ResourceType::Condensate)) {
                    continue;
                }
                if ((curResource != DataGlobalConstants::ResourceType::None)) {
                    int found = 0;
                    for (nUsePriceEsc = 1; nUsePriceEsc <= state.dataEconLifeCycleCost->numUsePriceEscalation; ++nUsePriceEsc) {
                        if (UsePriceEscalation(nUsePriceEsc).resource == curResource) {
                            found = nUsePriceEsc;
                            break;
                        }
                    }
                    if (found > 0) {
                        for (int jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
                            EscalatedEnergy.at(jYear).at(curResource) = CashFlow(iCashFlow).yrAmount(jYear) * UsePriceEscalation(found).Escalation(jYear);
                        }
                    } else { // if no escalation than just store the original energy cost
                        for (int jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
                            EscalatedEnergy.at(jYear).at(curResource) = CashFlow(iCashFlow).yrAmount(jYear);
                        }
                    }
                }
            }
        }
        for (auto kResource : DataGlobalConstants::AllResourceTypes) {
            for (int jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
                EscalatedTotEnergy(jYear) += EscalatedEnergy.at(jYear).at(kResource);
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

        // identify how each cashflow should be treated
        for (iCashFlow = 1; iCashFlow <= state.dataEconLifeCycleCost->numCashFlow; ++iCashFlow) {
            {
                auto const SELECT_CASE_var(CashFlow(iCashFlow).SourceKind);
                if (SELECT_CASE_var == skResource) {
                    // only for real fuels purchased such as electricity, natural gas, etc..
                    if ((CashFlow(iCashFlow).Resource >= DataGlobalConstants::ResourceType::Electricity) && (CashFlow(iCashFlow).Resource <= DataGlobalConstants::ResourceType::ElectricitySurplusSold)) {
                        CashFlow(iCashFlow).pvKind = pvkEnergy;
                    } else {
                        CashFlow(iCashFlow).pvKind = pvkNonEnergy;
                    }
                } else if ((SELECT_CASE_var == skRecurring) || (SELECT_CASE_var == skNonrecurring)) {
                    if (CashFlow(iCashFlow).Category == costCatEnergy) {
                        CashFlow(iCashFlow).pvKind = pvkEnergy;
                    } else {
                        CashFlow(iCashFlow).pvKind = pvkNonEnergy;
                    }
                } else if (SELECT_CASE_var == skSum) {
                    CashFlow(iCashFlow).pvKind = pvkNotComputed;
                } else {
                    CashFlow(iCashFlow).pvKind = pvkNotComputed;
                }
            }
        }
        // compute the Single Present Value factors based on the discount rate
        SPV.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
        for (int year = 1; year <= state.dataEconLifeCycleCost->lengthStudyYears; ++year) {
            std::map<DataGlobalConstants::ResourceType, Real64> yearMap;
            for (auto iResource : DataGlobalConstants::AllResourceTypes) {
                yearMap.insert(std::pair<DataGlobalConstants::ResourceType, Real64> (iResource, 0.0));
            }
            energySPV.insert(std::pair<int, std::map<DataGlobalConstants::ResourceType, Real64>>(year, yearMap));
        }

        // Depending if using Constant or Current Dollar analysis
        // use the appropriate discount rate
        if (state.dataEconLifeCycleCost->inflationApproach == iInflAppr::ConstantDollar) {
            curDiscountRate = state.dataEconLifeCycleCost->realDiscountRate;
        } else if (state.dataEconLifeCycleCost->inflationApproach == iInflAppr::CurrentDollar) {
            curDiscountRate = state.dataEconLifeCycleCost->nominalDiscountRate;
        }
        // compute single present values based on real discount rates
        for (jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
            // NIST 155 D.2.1.1 - Single Present Value (SPV) formula
            {
                auto const SELECT_CASE_var(state.dataEconLifeCycleCost->discountConvention);
                if (SELECT_CASE_var == iDiscConv::BeginOfYear) {
                    effectiveYear = double(jYear) - 1.0;
                } else if (SELECT_CASE_var == iDiscConv::MidYear) {
                    effectiveYear = double(jYear) - 0.5;
                } else if (SELECT_CASE_var == iDiscConv::EndOfYear) {
                    effectiveYear = double(jYear);
                } else {
                }
            }
            SPV(jYear) = 1.0 / std::pow(1.0 + curDiscountRate, effectiveYear);
        }
        // use SPV as default values for all energy types
        for (jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
            for (auto kResource : DataGlobalConstants::AllResourceTypes) {
                energySPV.at(jYear).at(kResource) = SPV(jYear);
            }
        }
        // loop through the resources and if they match a UseEscalation use those values instead
        for (nUsePriceEsc = 1; nUsePriceEsc <= state.dataEconLifeCycleCost->numUsePriceEscalation; ++nUsePriceEsc) {
            auto curResource = UsePriceEscalation(nUsePriceEsc).resource;
            if (curResource != DataGlobalConstants::ResourceType::None) {
                for (jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
                    // the following is based on UPV* formula from NIST 135 supplement but is for a single year
                    {
                        auto const SELECT_CASE_var(state.dataEconLifeCycleCost->discountConvention);
                        if (SELECT_CASE_var == iDiscConv::BeginOfYear) {
                            effectiveYear = double(jYear) - 1.0;
                        } else if (SELECT_CASE_var == iDiscConv::MidYear) {
                            effectiveYear = double(jYear) - 0.5;
                        } else if (SELECT_CASE_var == iDiscConv::EndOfYear) {
                            effectiveYear = double(jYear);
                        } else {
                        }
                    }
                    energySPV.at(jYear).at(curResource) =
                        UsePriceEscalation(nUsePriceEsc).Escalation(jYear) / std::pow(1.0 + curDiscountRate, effectiveYear);
                }
            }
        }
        for (iCashFlow = 1; iCashFlow <= state.dataEconLifeCycleCost->numCashFlow; ++iCashFlow) {
            {
                auto const SELECT_CASE_var(CashFlow(iCashFlow).pvKind);
                if (SELECT_CASE_var == pvkNonEnergy) {
                    totalPV = 0.0;
                    for (jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
                        CashFlow(iCashFlow).yrPresVal(jYear) = CashFlow(iCashFlow).yrAmount(jYear) * SPV(jYear);
                        totalPV += CashFlow(iCashFlow).yrPresVal(jYear);
                    }
                    CashFlow(iCashFlow).presentValue = totalPV;
                } else if (SELECT_CASE_var == pvkEnergy) {
                    auto curResource = CashFlow(iCashFlow).Resource;
                    if (curResource != DataGlobalConstants::ResourceType::None) {
                        totalPV = 0.0;
                        for (jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
                            CashFlow(iCashFlow).yrPresVal(jYear) = CashFlow(iCashFlow).yrAmount(jYear) * energySPV.at(jYear).at(curResource);
                            totalPV += CashFlow(iCashFlow).yrPresVal(jYear);
                        }
                        CashFlow(iCashFlow).presentValue = totalPV;
                    }
                } else if (SELECT_CASE_var == pvkNotComputed) {
                    // do nothing
                }
            }
        }
        // sum by category
        for (int i = 1; i <= countOfCostCat; ++i) {
            CashFlow(i).presentValue = 0; // initialize value to zero before summing in next for loop
        }
        for (iCashFlow = countOfCostCat + 1; iCashFlow <= state.dataEconLifeCycleCost->numCashFlow; ++iCashFlow) {
            curCategory = CashFlow(iCashFlow).Category;
            if ((curCategory <= countOfCostCat) && (curCategory >= 1)) {
                CashFlow(curCategory).presentValue += CashFlow(iCashFlow).presentValue;
                for (jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
                    CashFlow(curCategory).yrPresVal(jYear) += CashFlow(iCashFlow).yrPresVal(jYear);
                }
            }
        }
        // create total categories
        CashFlow(costCatTotEnergy).presentValue = CashFlow(costCatEnergy).presentValue;
        CashFlow(costCatTotOper).presentValue =
            CashFlow(costCatMaintenance).presentValue + CashFlow(costCatRepair).presentValue + CashFlow(costCatOperation).presentValue +
            CashFlow(costCatReplacement).presentValue + CashFlow(costCatMinorOverhaul).presentValue + CashFlow(costCatMajorOverhaul).presentValue +
            CashFlow(costCatOtherOperational).presentValue + CashFlow(costCatWater).presentValue + CashFlow(costCatEnergy).presentValue;
        CashFlow(costCatTotCaptl).presentValue =
            CashFlow(costCatConstruction).presentValue + CashFlow(costCatSalvage).presentValue + CashFlow(costCatOtherCapital).presentValue;
        CashFlow(costCatTotGrand).presentValue = CashFlow(costCatTotOper).presentValue + CashFlow(costCatTotCaptl).presentValue;
        for (jYear = 1; jYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++jYear) {
            CashFlow(costCatTotEnergy).yrPresVal(jYear) = CashFlow(costCatEnergy).yrPresVal(jYear);
            CashFlow(costCatTotOper).yrPresVal(jYear) = CashFlow(costCatMaintenance).yrPresVal(jYear) + CashFlow(costCatRepair).yrPresVal(jYear) +
                                                        CashFlow(costCatOperation).yrPresVal(jYear) + CashFlow(costCatReplacement).yrPresVal(jYear) +
                                                        CashFlow(costCatMinorOverhaul).yrPresVal(jYear) +
                                                        CashFlow(costCatMajorOverhaul).yrPresVal(jYear) +
                                                        CashFlow(costCatOtherOperational).yrPresVal(jYear) + CashFlow(costCatWater).yrPresVal(jYear) +
                                                        CashFlow(costCatEnergy).yrPresVal(jYear);
            CashFlow(costCatTotCaptl).yrPresVal(jYear) = CashFlow(costCatConstruction).yrPresVal(jYear) + CashFlow(costCatSalvage).yrPresVal(jYear) +
                                                         CashFlow(costCatOtherCapital).yrPresVal(jYear);
            CashFlow(costCatTotGrand).yrPresVal(jYear) = CashFlow(costCatTotOper).yrPresVal(jYear) + CashFlow(costCatTotCaptl).yrPresVal(jYear);
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
        int const SizeDepr(41);

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D<Real64> DepreciationPercent(SizeDepr); // values expressed as percent 5% is 5.0 (based on tables)
        Real64 curCapital;
        int curDepYear;
        int iYear;
        int jYear;

        DepreciatedCapital.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
        TaxableIncome.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
        Taxes.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
        AfterTaxCashFlow.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
        AfterTaxPresentValue.allocate(state.dataEconLifeCycleCost->lengthStudyYears);

        // Depreciation factors are based on IRS Publication 946 for 2009 "How to Depreciate Property"
        // The MACRS valus are based on Modified Accelerated Cost Recovery System GDS for 3, 5, 7, 10 year
        // property are based on 200% depreciation method shown in Appendix A using half year. 15 and 20 are
        // based on 150% (Chart 1). For Straight Line depreciation GDS is used for 27 years (actually 27.5)
        // 31 years (actually 31.5 years) and 39 years using mid month. For 40 years ADS is used (chart 2)
        // Table A-1 is used for 3, 4, 5, 10, 15 and 20 years. Table A-6 is for 27 years. Table A-7 for 31 years.
        // Table A-7a for 39 years. Table A-13 for 40 years. These years are a classification of property
        // and should not be confused with the length of the study. For 27 years, 31 years, 39 years and 40 years
        // the June value was used.
        DepreciationPercent = 0.0; // default all values to zero
        {
            auto const SELECT_CASE_var(state.dataEconLifeCycleCost->depreciationMethod);
            if (SELECT_CASE_var == iDeprMethod::MACRS3) { // IRS Publication 946 for 2009 Table A-1
                DepreciationPercent(1) = 33.33;
                DepreciationPercent(2) = 44.45;
                DepreciationPercent(3) = 14.81;
                DepreciationPercent(4) = 7.41;
            } else if (SELECT_CASE_var == iDeprMethod::MACRS5) { // IRS Publication 946 for 2009 Table A-1
                DepreciationPercent(1) = 20.0;
                DepreciationPercent(2) = 32.0;
                DepreciationPercent(3) = 19.2;
                DepreciationPercent(4) = 11.52;
                DepreciationPercent(5) = 11.52;
                DepreciationPercent(6) = 5.76;
            } else if (SELECT_CASE_var == iDeprMethod::MACRS7) { // IRS Publication 946 for 2009 Table A-1
                DepreciationPercent(1) = 14.29;
                DepreciationPercent(2) = 24.49;
                DepreciationPercent(3) = 17.49;
                DepreciationPercent(4) = 12.49;
                DepreciationPercent(5) = 8.93;
                DepreciationPercent(6) = 8.92;
                DepreciationPercent(7) = 8.93;
                DepreciationPercent(8) = 4.46;
            } else if (SELECT_CASE_var == iDeprMethod::MACRS10) { // IRS Publication 946 for 2009 Table A-1
                DepreciationPercent(1) = 10.0;
                DepreciationPercent(2) = 18.0;
                DepreciationPercent(3) = 14.4;
                DepreciationPercent(4) = 11.52;
                DepreciationPercent(5) = 9.22;
                DepreciationPercent(6) = 7.37;
                DepreciationPercent(7) = 6.55;
                DepreciationPercent(8) = 6.55;
                DepreciationPercent(9) = 6.56;
                DepreciationPercent(10) = 6.55;
                DepreciationPercent(11) = 3.28;
            } else if (SELECT_CASE_var == iDeprMethod::MACRS15) { // IRS Publication 946 for 2009 Table A-1
                DepreciationPercent(1) = 5.0;
                DepreciationPercent(2) = 9.5;
                DepreciationPercent(3) = 8.55;
                DepreciationPercent(4) = 7.7;
                DepreciationPercent(5) = 6.93;
                DepreciationPercent(6) = 6.23;
                DepreciationPercent(7) = 5.9;
                DepreciationPercent(8) = 5.9;
                DepreciationPercent(9) = 5.91;
                DepreciationPercent(10) = 5.9;
                DepreciationPercent(11) = 5.91;
                DepreciationPercent(12) = 5.9;
                DepreciationPercent(13) = 5.91;
                DepreciationPercent(14) = 5.9;
                DepreciationPercent(15) = 5.91;
                DepreciationPercent(16) = 2.95;
            } else if (SELECT_CASE_var == iDeprMethod::MACRS20) { // IRS Publication 946 for 2009 Table A-1
                DepreciationPercent(1) = 3.75;
                DepreciationPercent(2) = 7.219;
                DepreciationPercent(3) = 6.677;
                DepreciationPercent(4) = 6.177;
                DepreciationPercent(5) = 5.713;
                DepreciationPercent(6) = 5.285;
                DepreciationPercent(7) = 4.888;
                DepreciationPercent(8) = 4.522;
                DepreciationPercent(9) = 4.462;
                DepreciationPercent(10) = 4.461;
                DepreciationPercent(11) = 4.462;
                DepreciationPercent(12) = 4.461;
                DepreciationPercent(13) = 4.462;
                DepreciationPercent(14) = 4.461;
                DepreciationPercent(15) = 4.462;
                DepreciationPercent(16) = 4.461;
                DepreciationPercent(17) = 4.462;
                DepreciationPercent(18) = 4.461;
                DepreciationPercent(19) = 4.462;
                DepreciationPercent(20) = 4.461;
                DepreciationPercent(21) = 2.231;
            } else if (SELECT_CASE_var == iDeprMethod::Straight27) { // IRS Publication 946 for 2009 Table A-6 (June)
                DepreciationPercent(1) = 1.97;
                DepreciationPercent(2) = 3.636;
                DepreciationPercent(3) = 3.636;
                DepreciationPercent(4) = 3.636;
                DepreciationPercent(5) = 3.636;
                DepreciationPercent(6) = 3.636;
                DepreciationPercent(7) = 3.636;
                DepreciationPercent(8) = 3.636;
                DepreciationPercent(9) = 3.636;
                DepreciationPercent(10) = 3.637;
                DepreciationPercent(11) = 3.636;
                DepreciationPercent(12) = 3.637;
                DepreciationPercent(13) = 3.636;
                DepreciationPercent(14) = 3.637;
                DepreciationPercent(15) = 3.636;
                DepreciationPercent(16) = 3.637;
                DepreciationPercent(17) = 3.636;
                DepreciationPercent(18) = 3.637;
                DepreciationPercent(19) = 3.636;
                DepreciationPercent(20) = 3.637;
                DepreciationPercent(21) = 3.636;
                DepreciationPercent(22) = 3.637;
                DepreciationPercent(23) = 3.636;
                DepreciationPercent(24) = 3.637;
                DepreciationPercent(25) = 3.636;
                DepreciationPercent(26) = 3.637;
                DepreciationPercent(27) = 3.636;
                DepreciationPercent(28) = 3.485;
            } else if (SELECT_CASE_var == iDeprMethod::Straight31) { // IRS Publication 946 for 2009 Table A-7 (June)
                DepreciationPercent(1) = 1.72;
                DepreciationPercent(2) = 3.175;
                DepreciationPercent(3) = 3.175;
                DepreciationPercent(4) = 3.175;
                DepreciationPercent(5) = 3.175;
                DepreciationPercent(6) = 3.175;
                DepreciationPercent(7) = 3.175;
                DepreciationPercent(8) = 3.174;
                DepreciationPercent(9) = 3.175;
                DepreciationPercent(10) = 3.174;
                DepreciationPercent(11) = 3.175;
                DepreciationPercent(12) = 3.174;
                DepreciationPercent(13) = 3.175;
                DepreciationPercent(14) = 3.174;
                DepreciationPercent(15) = 3.175;
                DepreciationPercent(16) = 3.174;
                DepreciationPercent(17) = 3.175;
                DepreciationPercent(18) = 3.174;
                DepreciationPercent(19) = 3.175;
                DepreciationPercent(20) = 3.174;
                DepreciationPercent(21) = 3.175;
                DepreciationPercent(22) = 3.174;
                DepreciationPercent(23) = 3.175;
                DepreciationPercent(24) = 3.174;
                DepreciationPercent(25) = 3.175;
                DepreciationPercent(26) = 3.174;
                DepreciationPercent(27) = 3.175;
                DepreciationPercent(28) = 3.174;
                DepreciationPercent(29) = 3.175;
                DepreciationPercent(30) = 3.174;
                DepreciationPercent(31) = 3.175;
                DepreciationPercent(32) = 3.042;
            } else if (SELECT_CASE_var == iDeprMethod::Straight39) { // IRS Publication 946 for 2009 Table A-7a (June)
                DepreciationPercent(1) = 1.391;
                DepreciationPercent(2) = 2.564;
                DepreciationPercent(3) = 2.564;
                DepreciationPercent(4) = 2.564;
                DepreciationPercent(5) = 2.564;
                DepreciationPercent(6) = 2.564;
                DepreciationPercent(7) = 2.564;
                DepreciationPercent(8) = 2.564;
                DepreciationPercent(9) = 2.564;
                DepreciationPercent(10) = 2.564;
                DepreciationPercent(11) = 2.564;
                DepreciationPercent(12) = 2.564;
                DepreciationPercent(13) = 2.564;
                DepreciationPercent(14) = 2.564;
                DepreciationPercent(15) = 2.564;
                DepreciationPercent(16) = 2.564;
                DepreciationPercent(17) = 2.564;
                DepreciationPercent(18) = 2.564;
                DepreciationPercent(19) = 2.564;
                DepreciationPercent(20) = 2.564;
                DepreciationPercent(21) = 2.564;
                DepreciationPercent(22) = 2.564;
                DepreciationPercent(23) = 2.564;
                DepreciationPercent(24) = 2.564;
                DepreciationPercent(25) = 2.564;
                DepreciationPercent(26) = 2.564;
                DepreciationPercent(27) = 2.564;
                DepreciationPercent(28) = 2.564;
                DepreciationPercent(29) = 2.564;
                DepreciationPercent(30) = 2.564;
                DepreciationPercent(31) = 2.564;
                DepreciationPercent(32) = 2.564;
                DepreciationPercent(33) = 2.564;
                DepreciationPercent(34) = 2.564;
                DepreciationPercent(35) = 2.564;
                DepreciationPercent(36) = 2.564;
                DepreciationPercent(37) = 2.564;
                DepreciationPercent(38) = 2.564;
                DepreciationPercent(39) = 2.564;
                DepreciationPercent(40) = 1.177;
            } else if (SELECT_CASE_var == iDeprMethod::Straight40) { // IRS Publication 946 for 2009 Table A-13 (June)
                DepreciationPercent(1) = 1.354;
                DepreciationPercent(2) = 2.5;
                DepreciationPercent(3) = 2.5;
                DepreciationPercent(4) = 2.5;
                DepreciationPercent(5) = 2.5;
                DepreciationPercent(6) = 2.5;
                DepreciationPercent(7) = 2.5;
                DepreciationPercent(8) = 2.5;
                DepreciationPercent(9) = 2.5;
                DepreciationPercent(10) = 2.5;
                DepreciationPercent(11) = 2.5;
                DepreciationPercent(12) = 2.5;
                DepreciationPercent(13) = 2.5;
                DepreciationPercent(14) = 2.5;
                DepreciationPercent(15) = 2.5;
                DepreciationPercent(16) = 2.5;
                DepreciationPercent(17) = 2.5;
                DepreciationPercent(18) = 2.5;
                DepreciationPercent(19) = 2.5;
                DepreciationPercent(20) = 2.5;
                DepreciationPercent(21) = 2.5;
                DepreciationPercent(22) = 2.5;
                DepreciationPercent(23) = 2.5;
                DepreciationPercent(24) = 2.5;
                DepreciationPercent(25) = 2.5;
                DepreciationPercent(26) = 2.5;
                DepreciationPercent(27) = 2.5;
                DepreciationPercent(28) = 2.5;
                DepreciationPercent(29) = 2.5;
                DepreciationPercent(30) = 2.5;
                DepreciationPercent(31) = 2.5;
                DepreciationPercent(32) = 2.5;
                DepreciationPercent(33) = 2.5;
                DepreciationPercent(34) = 2.5;
                DepreciationPercent(35) = 2.5;
                DepreciationPercent(36) = 2.5;
                DepreciationPercent(37) = 2.5;
                DepreciationPercent(38) = 2.5;
                DepreciationPercent(39) = 2.5;
                DepreciationPercent(40) = 2.5;
                DepreciationPercent(41) = 1.146;
            }
        }
        // convert construction costs (not salvage) into depreciation
        DepreciatedCapital = 0.0; // set all years to zero
        for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
            curCapital = CashFlow(costCatConstruction).yrAmount(iYear) + CashFlow(costCatOtherCapital).yrAmount(iYear);
            for (jYear = 1; jYear <= SizeDepr; ++jYear) {
                curDepYear = iYear + jYear - 1; // start depreciating with the year that the capital was shown and go to years following
                if (curDepYear <= state.dataEconLifeCycleCost->lengthStudyYears) {
                    DepreciatedCapital(curDepYear) += curCapital * (DepreciationPercent(jYear) / 100);
                }
            }
        }
        // Using Newnan pg 3880
        //   before-tax cash flow
        //   depreciation
        //   taxable income (before-tax cash flow - depreciation)
        //   income taxes (taxable income x incremental tax rate)
        //   after-tax cash flow (before-tax cash flow - income taxes)
        for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
            TaxableIncome(iYear) = CashFlow(costCatTotGrand).yrAmount(iYear) - DepreciatedCapital(iYear);
            Taxes(iYear) = TaxableIncome(iYear) * state.dataEconLifeCycleCost->taxRate;
            AfterTaxCashFlow(iYear) = CashFlow(costCatTotGrand).yrAmount(iYear) - Taxes(iYear);
            // the present value after taxes is pretax present value minus the present value of the taxes
            AfterTaxPresentValue(iYear) = CashFlow(costCatTotGrand).yrPresVal(iYear) - Taxes(iYear) * SPV(iYear);
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

        if (state.dataEconLifeCycleCost->LCCparamPresent && state.dataOutRptTab->displayLifeCycleCostReport) {
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

            tableBody(1, 1) = state.dataEconLifeCycleCost->LCCname;
            if (state.dataEconLifeCycleCost->discountConvention == iDiscConv::EndOfYear) {
                tableBody(1, 2) = "EndOfYear";
            } else if (state.dataEconLifeCycleCost->discountConvention == iDiscConv::MidYear) {
                tableBody(1, 2) = "MidYear";
            } else if (state.dataEconLifeCycleCost->discountConvention == iDiscConv::BeginOfYear) {
                tableBody(1, 2) = "BeginningOfYear";
            }
            if (state.dataEconLifeCycleCost->inflationApproach == iInflAppr::ConstantDollar) {
                tableBody(1, 3) = "ConstantDollar";
            } else if (state.dataEconLifeCycleCost->inflationApproach == iInflAppr::CurrentDollar) {
                tableBody(1, 3) = "CurrentDollar";
            }
            if (state.dataEconLifeCycleCost->inflationApproach == iInflAppr::ConstantDollar) {
                tableBody(1, 4) = RealToStr(state.dataEconLifeCycleCost->realDiscountRate, 4);
            } else {
                tableBody(1, 4) = "-- N/A --";
            }
            if (state.dataEconLifeCycleCost->inflationApproach == iInflAppr::CurrentDollar) {
                tableBody(1, 5) = RealToStr(state.dataEconLifeCycleCost->nominalDiscountRate, 4);
            } else {
                tableBody(1, 5) = "-- N/A --";
            }
            if (state.dataEconLifeCycleCost->inflationApproach == iInflAppr::CurrentDollar) {
                tableBody(1, 6) = RealToStr(state.dataEconLifeCycleCost->inflation, 4);
            } else {
                tableBody(1, 6) = "-- N/A --";
            }
            tableBody(1, 7) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->baseDateMonth), state.dataEconLifeCycleCost->baseDateYear);
            tableBody(1, 8) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->serviceDateMonth), state.dataEconLifeCycleCost->serviceDateYear);
            tableBody(1, 9) = fmt::to_string(state.dataEconLifeCycleCost->lengthStudyYears);
            tableBody(1, 10) = RealToStr(state.dataEconLifeCycleCost->taxRate, 4);
            {
                auto const SELECT_CASE_var(state.dataEconLifeCycleCost->depreciationMethod);
                if (SELECT_CASE_var == iDeprMethod::MACRS3) {
                    tableBody(1, 11) = "ModifiedAcceleratedCostRecoverySystem-3year";
                } else if (SELECT_CASE_var == iDeprMethod::MACRS5) {
                    tableBody(1, 11) = "ModifiedAcceleratedCostRecoverySystem-5year";
                } else if (SELECT_CASE_var == iDeprMethod::MACRS7) {
                    tableBody(1, 11) = "ModifiedAcceleratedCostRecoverySystem-7year";
                } else if (SELECT_CASE_var == iDeprMethod::MACRS10) {
                    tableBody(1, 11) = "ModifiedAcceleratedCostRecoverySystem-10year";
                } else if (SELECT_CASE_var == iDeprMethod::MACRS15) {
                    tableBody(1, 11) = "ModifiedAcceleratedCostRecoverySystem-15year";
                } else if (SELECT_CASE_var == iDeprMethod::MACRS20) {
                    tableBody(1, 11) = "ModifiedAcceleratedCostRecoverySystem-20year";
                } else if (SELECT_CASE_var == iDeprMethod::Straight27) {
                    tableBody(1, 11) = "StraightLine-27year";
                } else if (SELECT_CASE_var == iDeprMethod::Straight31) {
                    tableBody(1, 11) = "StraightLine-31year";
                } else if (SELECT_CASE_var == iDeprMethod::Straight39) {
                    tableBody(1, 11) = "StraightLine-39year";
                } else if (SELECT_CASE_var == iDeprMethod::Straight40) {
                    tableBody(1, 11) = "StraightLine-40year";
                } else if (SELECT_CASE_var == iDeprMethod::None) {
                    tableBody(1, 11) = "None";
                }
            }
            columnWidth = 14; // array assignment - same for all columns
            WriteSubtitle(state, "Life-Cycle Cost Parameters");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Life-Cycle Cost Parameters");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Life-Cycle Cost Parameters");
            }

            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
            //---- Use Price Escalation
            numColumns = max(1, state.dataEconLifeCycleCost->numUsePriceEscalation);
            rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears + 2);
            columnHead.allocate(numColumns);
            columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
            tableBody.allocate(numColumns, state.dataEconLifeCycleCost->lengthStudyYears + 2);
            tableBody = "";
            columnHead = "none";
            rowHead(1) = "Resource";
            rowHead(2) = "Start Date";
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                rowHead(iYear + 2) = fmt::to_string(iYear);
            }
            for (jObj = 1; jObj <= state.dataEconLifeCycleCost->numUsePriceEscalation; ++jObj) { // loop through objects not columns to add names
                columnHead(jObj) = UsePriceEscalation(jObj).name;
                tableBody(jObj, 1) = GetResourceTypeChar(UsePriceEscalation(jObj).resource);
                tableBody(jObj, 2) =
                    format("{} {}", MonthNames(UsePriceEscalation(jObj).escalationStartMonth), UsePriceEscalation(jObj).escalationStartYear);
            }
            for (jObj = 1; jObj <= state.dataEconLifeCycleCost->numUsePriceEscalation; ++jObj) {
                for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                    tableBody(jObj, iYear + 2) = RealToStr(UsePriceEscalation(jObj).Escalation(iYear), 6);
                }
            }
            WriteSubtitle(state, "Use Price Escalation");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Price Escalation");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Price Escalation");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
            //---- Use Adjustment
            if (state.dataEconLifeCycleCost->numUseAdjustment >= 1) { // only create table if objects used
                numColumns = max(1, state.dataEconLifeCycleCost->numUseAdjustment);
                numYears = state.dataEconLifeCycleCost->lengthStudyYears - (state.dataEconLifeCycleCost->serviceDateYear - state.dataEconLifeCycleCost->baseDateYear);
                rowHead.allocate(numYears + 1);
                columnHead.allocate(numColumns);
                columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
                tableBody.allocate(numColumns, numYears + 1);
                tableBody = "";
                columnHead = "none";
                rowHead(1) = "";
                for (iYear = 1; iYear <= numYears; ++iYear) {
                    rowHead(iYear + 1) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->serviceDateMonth), state.dataEconLifeCycleCost->serviceDateYear + iYear - 1);
                }
                for (jObj = 1; jObj <= state.dataEconLifeCycleCost->numUseAdjustment; ++jObj) { // loop through objects not columns to add names
                    columnHead(jObj) = UseAdjustment(jObj).name;
                    tableBody(jObj, 1) = GetResourceTypeChar(UseAdjustment(jObj).resource);
                }
                for (jObj = 1; jObj <= state.dataEconLifeCycleCost->numUseAdjustment; ++jObj) {
                    for (iYear = 1; iYear <= numYears; ++iYear) {
                        tableBody(jObj, iYear + 1) = RealToStr(UseAdjustment(jObj).Adjustment(iYear), 6);
                    }
                }
                WriteSubtitle(state, "Use Adjustment");
                WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
                if (sqlite) {
                    sqlite->createSQLiteTabularDataRecords(
                        tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Adjustment");
                }
                if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                    ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
                        tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Adjustment");
                }
                columnHead.deallocate();
                rowHead.deallocate();
                columnWidth.deallocate();
                tableBody.deallocate();
            }
            //---- Cash Flow for Recurring and Nonrecurring Costs
            numColumns = max(1, state.dataEconLifeCycleCost->numRecurringCosts + state.dataEconLifeCycleCost->numNonrecurringCost);
            rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears + 1);
            columnHead.allocate(numColumns);
            columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
            tableBody.allocate(numColumns, state.dataEconLifeCycleCost->lengthStudyYears + 1);
            tableBody = "";
            rowHead(1) = "";
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                rowHead(iYear + 1) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->baseDateMonth), state.dataEconLifeCycleCost->baseDateYear + iYear - 1);
            }
            for (jObj = 1; jObj <= (state.dataEconLifeCycleCost->numRecurringCosts + state.dataEconLifeCycleCost->numNonrecurringCost); ++jObj) {
                curCashFlow = countOfCostCat + jObj;
                columnHead(jObj) = CashFlow(curCashFlow).name;
                {
                    auto const SELECT_CASE_var(CashFlow(curCashFlow).SourceKind);
                    if (SELECT_CASE_var == skNonrecurring) {
                        tableBody(jObj, 1) = "Nonrecurring";
                    } else if (SELECT_CASE_var == skRecurring) {
                        tableBody(jObj, 1) = "Recurring";
                    }
                }
                for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                    tableBody(jObj, iYear + 1) = RealToStr(CashFlow(curCashFlow).yrAmount(iYear), 2);
                }
            }
            WriteSubtitle(state, "Cash Flow for Recurring and Nonrecurring Costs (Without Escalation)");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(tableBody,
                                                       rowHead,
                                                       columnHead,
                                                       "Life-Cycle Cost Report",
                                                       "Entire Facility",
                                                       "Cash Flow for Recurring and Nonrecurring Costs (Without Escalation)");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
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
            numColumns = max(1, state.dataEconLifeCycleCost->numResourcesUsed + 1);
            rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
            columnHead.allocate(numColumns);
            columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
            tableBody.allocate(numColumns, state.dataEconLifeCycleCost->lengthStudyYears);
            tableBody = "";
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                rowHead(iYear) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->baseDateMonth), state.dataEconLifeCycleCost->baseDateYear + iYear - 1);
            }
            for (jObj = 1; jObj <= state.dataEconLifeCycleCost->numResourcesUsed; ++jObj) {
                curCashFlow = countOfCostCat + state.dataEconLifeCycleCost->numRecurringCosts + state.dataEconLifeCycleCost->numNonrecurringCost + jObj;
                columnHead(jObj) = CashFlow(curCashFlow).name;
                for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                    tableBody(jObj, iYear) = RealToStr(CashFlow(curCashFlow).yrAmount(iYear), 2);
                }
            }
            columnHead(numColumns) = "Total";
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                tableBody(jObj, iYear) = RealToStr(CashFlow(costCatTotEnergy).yrAmount(iYear) + CashFlow(costCatWater).yrAmount(iYear), 2);
            }
            WriteSubtitle(state, "Energy and Water Cost Cash Flows (Without Escalation)");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(tableBody,
                                                       rowHead,
                                                       columnHead,
                                                       "Life-Cycle Cost Report",
                                                       "Entire Facility",
                                                       "Energy and Water Cost Cash Flows (Without Escalation)");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(tableBody,
                                                                                            rowHead,
                                                                                            columnHead,
                                                                                            "Life-Cycle Cost Report",
                                                                                            "Entire Facility",
                                                                                            "Energy and Water Cost Cash Flows (Without Escalation)");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
            //---- Energy and Water Cost Cash Flows (With Escalation)
            numColumns = max(1, state.dataEconLifeCycleCost->numResourcesUsed + 1);
            rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
            columnHead.allocate(numColumns);
            columnWidth.dimension(numColumns, 14); // array assignment - same for all columns
            tableBody.allocate(numColumns, state.dataEconLifeCycleCost->lengthStudyYears);
            tableBody = "";
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                rowHead(iYear) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->baseDateMonth), state.dataEconLifeCycleCost->baseDateYear + iYear - 1);
            }
            for (jObj = 1; jObj <= state.dataEconLifeCycleCost->numResourcesUsed; ++jObj) {
                curCashFlow = countOfCostCat + state.dataEconLifeCycleCost->numRecurringCosts + state.dataEconLifeCycleCost->numNonrecurringCost + jObj;
                columnHead(jObj) = CashFlow(curCashFlow).name;
                auto curResource = CashFlow(curCashFlow).Resource;
                if (CashFlow(curCashFlow).Resource != DataGlobalConstants::ResourceType::Water) {
                    for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                        tableBody(jObj, iYear) = RealToStr(EscalatedEnergy.at(iYear).at(curResource), 2);
                    }
                } else { // for water just use the original cashflow since not involved in escalation
                    for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                        tableBody(jObj, iYear) = RealToStr(CashFlow(curCashFlow).yrAmount(iYear), 2);
                    }
                }
            }
            columnHead(numColumns) = "Total";
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                tableBody(jObj, iYear) = RealToStr(EscalatedTotEnergy(iYear) + CashFlow(costCatWater).yrAmount(iYear), 2);
            }
            WriteSubtitle(state, "Energy and Water Cost Cash Flows (With Escalation)");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(tableBody,
                                                       rowHead,
                                                       columnHead,
                                                       "Life-Cycle Cost Report",
                                                       "Entire Facility",
                                                       "Energy and Water Cost Cash Flows (With Escalation)");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(tableBody,
                                                                                            rowHead,
                                                                                            columnHead,
                                                                                            "Life-Cycle Cost Report",
                                                                                            "Entire Facility",
                                                                                            "Energy and Water Cost Cash Flows (With Escalation)");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();

            //---- Capital Cash Flow by Category
            rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
            columnHead.allocate(4);
            columnWidth.allocate(4);
            columnWidth = 14; // array assignment - same for all columns
            tableBody.allocate(4, state.dataEconLifeCycleCost->lengthStudyYears);
            tableBody = "";
            columnHead(1) = "Construction";
            columnHead(2) = "Salvage";
            columnHead(3) = "OtherCapital";
            columnHead(4) = "Total";
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                rowHead(iYear) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->baseDateMonth), state.dataEconLifeCycleCost->baseDateYear + iYear - 1);
                tableBody(1, iYear) = RealToStr(CashFlow(costCatConstruction).yrAmount(iYear), 2);
                tableBody(2, iYear) = RealToStr(CashFlow(costCatSalvage).yrAmount(iYear), 2);
                tableBody(3, iYear) = RealToStr(CashFlow(costCatOtherCapital).yrAmount(iYear), 2);
                tableBody(4, iYear) = RealToStr(CashFlow(costCatTotCaptl).yrAmount(iYear), 2);
            }
            WriteSubtitle(state, "Capital Cash Flow by Category (Without Escalation)");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(tableBody,
                                                       rowHead,
                                                       columnHead,
                                                       "Life-Cycle Cost Report",
                                                       "Entire Facility",
                                                       "Capital Cash Flow by Category (Without Escalation)");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(tableBody,
                                                                                            rowHead,
                                                                                            columnHead,
                                                                                            "Life-Cycle Cost Report",
                                                                                            "Entire Facility",
                                                                                            "Capital Cash Flow by Category (Without Escalation)");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
            //---- Operating Cash Flow by Category (Without Escalation)
            rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
            columnHead.allocate(10);
            columnWidth.allocate(10);
            columnWidth = 14; // array assignment - same for all columns
            tableBody.allocate(10, state.dataEconLifeCycleCost->lengthStudyYears);
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

            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                rowHead(iYear) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->baseDateMonth), state.dataEconLifeCycleCost->baseDateYear + iYear - 1);
                tableBody(1, iYear) = RealToStr(CashFlow(costCatEnergy).yrAmount(iYear), 2);
                tableBody(2, iYear) = RealToStr(CashFlow(costCatWater).yrAmount(iYear), 2);
                tableBody(3, iYear) = RealToStr(CashFlow(costCatMaintenance).yrAmount(iYear), 2);
                tableBody(4, iYear) = RealToStr(CashFlow(costCatRepair).yrAmount(iYear), 2);
                tableBody(5, iYear) = RealToStr(CashFlow(costCatOperation).yrAmount(iYear), 2);
                tableBody(6, iYear) = RealToStr(CashFlow(costCatReplacement).yrAmount(iYear), 2);
                tableBody(7, iYear) = RealToStr(CashFlow(costCatMinorOverhaul).yrAmount(iYear), 2);
                tableBody(8, iYear) = RealToStr(CashFlow(costCatMajorOverhaul).yrAmount(iYear), 2);
                tableBody(9, iYear) = RealToStr(CashFlow(costCatOtherOperational).yrAmount(iYear), 2);
                tableBody(10, iYear) = RealToStr(CashFlow(costCatTotOper).yrAmount(iYear), 2);
            }
            WriteSubtitle(state, "Operating Cash Flow by Category (Without Escalation)");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(tableBody,
                                                       rowHead,
                                                       columnHead,
                                                       "Life-Cycle Cost Report",
                                                       "Entire Facility",
                                                       "Operating Cash Flow by Category (Without Escalation)");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(tableBody,
                                                                                            rowHead,
                                                                                            columnHead,
                                                                                            "Life-Cycle Cost Report",
                                                                                            "Entire Facility",
                                                                                            "Operating Cash Flow by Category (Without Escalation)");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
            //---- Operating Cash Flow by Category (With Escalation)
            rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
            columnHead.allocate(10);
            columnWidth.allocate(10);
            columnWidth = 14; // array assignment - same for all columns
            tableBody.allocate(10, state.dataEconLifeCycleCost->lengthStudyYears);
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

            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                rowHead(iYear) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->baseDateMonth), state.dataEconLifeCycleCost->baseDateYear + iYear - 1);
                tableBody(1, iYear) = RealToStr(EscalatedTotEnergy(iYear), 2);
                tableBody(2, iYear) = RealToStr(CashFlow(costCatWater).yrAmount(iYear), 2);
                tableBody(3, iYear) = RealToStr(CashFlow(costCatMaintenance).yrAmount(iYear), 2);
                tableBody(4, iYear) = RealToStr(CashFlow(costCatRepair).yrAmount(iYear), 2);
                tableBody(5, iYear) = RealToStr(CashFlow(costCatOperation).yrAmount(iYear), 2);
                tableBody(6, iYear) = RealToStr(CashFlow(costCatReplacement).yrAmount(iYear), 2);
                tableBody(7, iYear) = RealToStr(CashFlow(costCatMinorOverhaul).yrAmount(iYear), 2);
                tableBody(8, iYear) = RealToStr(CashFlow(costCatMajorOverhaul).yrAmount(iYear), 2);
                tableBody(9, iYear) = RealToStr(CashFlow(costCatOtherOperational).yrAmount(iYear), 2);
                Real64 yearly_total_cost =
                    CashFlow(costCatTotOper).yrAmount(iYear) + EscalatedTotEnergy(iYear) - CashFlow(costCatTotEnergy).yrAmount(iYear);
                tableBody(10, iYear) = RealToStr(yearly_total_cost, 2);
            }
            WriteSubtitle(state, "Operating Cash Flow by Category (With Escalation)");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(tableBody,
                                                       rowHead,
                                                       columnHead,
                                                       "Life-Cycle Cost Report",
                                                       "Entire Facility",
                                                       "Operating Cash Flow by Category (With Escalation)");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(tableBody,
                                                                                            rowHead,
                                                                                            columnHead,
                                                                                            "Life-Cycle Cost Report",
                                                                                            "Entire Facility",
                                                                                            "Operating Cash Flow by Category (With Escalation)");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
            //---- DEBUG ONLY - Monthly Cash Flows
            bool showMonthlyCashFlows = false;
            if (showMonthlyCashFlows) {
                rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyTotalMonths);
                columnHead.allocate(state.dataEconLifeCycleCost->numCashFlow);
                columnWidth.allocate(state.dataEconLifeCycleCost->numCashFlow);
                tableBody.allocate(state.dataEconLifeCycleCost->numCashFlow, state.dataEconLifeCycleCost->lengthStudyTotalMonths);
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
                for (jObj = countOfCostCat + 1; jObj <= state.dataEconLifeCycleCost->numCashFlow; ++jObj) {
                    columnHead(jObj) = CashFlow(jObj).name;
                }
                for (kMonth = 1; kMonth <= state.dataEconLifeCycleCost->lengthStudyTotalMonths; ++kMonth) {
                    rowHead(kMonth) = format("{} {}", MonthNames(1 + (kMonth + state.dataEconLifeCycleCost->baseDateMonth - 2) % 12), state.dataEconLifeCycleCost->baseDateYear + int((kMonth - 1) / 12));
                }
                for (kMonth = 1; kMonth <= state.dataEconLifeCycleCost->lengthStudyTotalMonths; ++kMonth) {
                    for (jObj = 1; jObj <= state.dataEconLifeCycleCost->numCashFlow; ++jObj) {
                        tableBody(jObj, kMonth) = RealToStr(CashFlow(jObj).mnAmount(kMonth), 2);
                    }
                }
                WriteSubtitle(state, "DEBUG ONLY - Monthly Cash Flows");
                WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
                if (sqlite) {
                    sqlite->createSQLiteTabularDataRecords(
                        tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "DEBUG ONLY - Monthly Cash Flows");
                }
                if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                    ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
                        tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "DEBUG ONLY - Monthly Cash Flows");
                }
                columnHead.deallocate();
                rowHead.deallocate();
                columnWidth.deallocate();
                tableBody.deallocate();
            }
            //---- Monthly Total Cash Flow
            rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears);
            columnHead.allocate(12);
            columnWidth.allocate(12);
            columnWidth = 14; // array assignment - same for all columns
            tableBody.allocate(12, state.dataEconLifeCycleCost->lengthStudyYears);
            tableBody = "";
            for (kMonth = 1; kMonth <= 12; ++kMonth) {
                columnHead(kMonth) = MonthNames(kMonth);
            }
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                rowHead(iYear) = fmt::to_string(state.dataEconLifeCycleCost->baseDateYear + iYear - 1);
            }
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                for (kMonth = 1; kMonth <= 12; ++kMonth) {
                    tableBody(kMonth, iYear) = RealToStr(CashFlow(costCatTotGrand).mnAmount((iYear - 1) * 12 + kMonth), 2);
                }
            }
            WriteSubtitle(state, "Monthly Total Cash Flow (Without Escalation)");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Monthly Total Cash Flow (Without Escalation)");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Monthly Total Cash Flow (Without Escalation)");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
            //---- Present Value for Recurring, Nonrecurring and Energy Costs
            numRows = max(1, state.dataEconLifeCycleCost->numRecurringCosts + state.dataEconLifeCycleCost->numNonrecurringCost + state.dataEconLifeCycleCost->numResourcesUsed);
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
            for (jObj = 1; jObj <= (state.dataEconLifeCycleCost->numRecurringCosts + state.dataEconLifeCycleCost->numNonrecurringCost + state.dataEconLifeCycleCost->numResourcesUsed); ++jObj) {
                offset = countOfCostCat;
                rowHead(jObj) = CashFlow(offset + jObj).name;
                {
                    auto const SELECT_CASE_var(CashFlow(offset + jObj).Category);
                    if (SELECT_CASE_var == costCatMaintenance) {
                        tableBody(1, jObj) = "Maintenance";
                    } else if (SELECT_CASE_var == costCatRepair) {
                        tableBody(1, jObj) = "Repair";
                    } else if (SELECT_CASE_var == costCatOperation) {
                        tableBody(1, jObj) = "Operation";
                    } else if (SELECT_CASE_var == costCatReplacement) {
                        tableBody(1, jObj) = "Replacement";
                    } else if (SELECT_CASE_var == costCatMinorOverhaul) {
                        tableBody(1, jObj) = "Minor Overhaul";
                    } else if (SELECT_CASE_var == costCatMajorOverhaul) {
                        tableBody(1, jObj) = "Major Overhaul";
                    } else if (SELECT_CASE_var == costCatOtherOperational) {
                        tableBody(1, jObj) = "Other Operational";
                    } else if (SELECT_CASE_var == costCatConstruction) {
                        tableBody(1, jObj) = "Construction";
                    } else if (SELECT_CASE_var == costCatSalvage) {
                        tableBody(1, jObj) = "Salvage";
                    } else if (SELECT_CASE_var == costCatOtherCapital) {
                        tableBody(1, jObj) = "Other Capital";
                    } else if (SELECT_CASE_var == costCatWater) {
                        tableBody(1, jObj) = "Water";
                    } else if (SELECT_CASE_var == costCatEnergy) {
                        tableBody(1, jObj) = "Energy";
                    } else {
                        tableBody(1, jObj) = "-";
                    }
                }
                {
                    auto const SELECT_CASE_var(CashFlow(offset + jObj).SourceKind);
                    if (SELECT_CASE_var == skNonrecurring) {
                        tableBody(2, jObj) = "Nonrecurring";
                    } else if (SELECT_CASE_var == skRecurring) {
                        tableBody(2, jObj) = "Recurring";
                    } else if (SELECT_CASE_var == skResource) {
                        if (CashFlow(offset + jObj).Category == costCatWater) {
                            tableBody(2, jObj) = "Water Cost";
                        } else {
                            tableBody(2, jObj) = "Energy Cost";
                        }
                    } else {
                        tableBody(2, jObj) = "-";
                    }
                }
                tableBody(3, jObj) = RealToStr(CashFlow(offset + jObj).orginalCost, 2);
                tableBody(4, jObj) = RealToStr(CashFlow(offset + jObj).presentValue, 2);
                totalPV += CashFlow(offset + jObj).presentValue;
                if (CashFlow(offset + jObj).orginalCost != 0.0) {
                    tableBody(5, jObj) = RealToStr(CashFlow(offset + jObj).presentValue / CashFlow(offset + jObj).orginalCost, 4);
                } else {
                    tableBody(5, jObj) = "-";
                }
            }
            tableBody(4, numRows + 1) = RealToStr(totalPV, 2);
            WriteSubtitle(state, "Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(tableBody,
                                                       rowHead,
                                                       columnHead,
                                                       "Life-Cycle Cost Report",
                                                       "Entire Facility",
                                                       "Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
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

            tableBody(1, 1) = RealToStr(CashFlow(costCatConstruction).presentValue, 2);
            tableBody(1, 2) = RealToStr(CashFlow(costCatSalvage).presentValue, 2);
            tableBody(1, 3) = RealToStr(CashFlow(costCatOtherCapital).presentValue, 2);
            tableBody(1, 4) = RealToStr(CashFlow(costCatEnergy).presentValue, 2);
            tableBody(1, 5) = RealToStr(CashFlow(costCatWater).presentValue, 2);
            tableBody(1, 6) = RealToStr(CashFlow(costCatMaintenance).presentValue, 2);
            tableBody(1, 7) = RealToStr(CashFlow(costCatRepair).presentValue, 2);
            tableBody(1, 8) = RealToStr(CashFlow(costCatOperation).presentValue, 2);
            tableBody(1, 9) = RealToStr(CashFlow(costCatReplacement).presentValue, 2);
            tableBody(1, 10) = RealToStr(CashFlow(costCatMinorOverhaul).presentValue, 2);
            tableBody(1, 11) = RealToStr(CashFlow(costCatMajorOverhaul).presentValue, 2);
            tableBody(1, 12) = RealToStr(CashFlow(costCatOtherOperational).presentValue, 2);
            tableBody(1, 13) = RealToStr(CashFlow(costCatTotEnergy).presentValue, 2);
            tableBody(1, 14) = RealToStr(CashFlow(costCatTotOper).presentValue, 2);
            tableBody(1, 15) = RealToStr(CashFlow(costCatTotCaptl).presentValue, 2);
            tableBody(1, 16) = RealToStr(CashFlow(costCatTotGrand).presentValue, 2);

            WriteSubtitle(state, "Present Value by Category");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Category");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Category");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
            //---- Present Value by Year
            rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears + 1);
            columnHead.allocate(3);
            columnWidth.allocate(3);
            columnWidth = 14; // array assignment - same for all columns
            tableBody.allocate(3, state.dataEconLifeCycleCost->lengthStudyYears + 1);
            tableBody = "";
            columnHead(1) = "Total Cost (Without Escalation)";
            columnHead(2) = "Total Cost (With Escalation)";
            columnHead(3) = "Present Value of Costs";

            totalPV = 0.0;
            for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                rowHead(iYear) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->baseDateMonth), state.dataEconLifeCycleCost->baseDateYear + iYear - 1);
                tableBody(1, iYear) = RealToStr(CashFlow(costCatTotGrand).yrAmount(iYear), 2);
                // adjust for escalated energy costs
                Real64 yearly_total_cost =
                    CashFlow(costCatTotGrand).yrAmount(iYear) + EscalatedTotEnergy(iYear) - CashFlow(costCatTotEnergy).yrAmount(iYear);
                tableBody(2, iYear) = RealToStr(yearly_total_cost, 2);
                tableBody(3, iYear) = RealToStr(CashFlow(costCatTotGrand).yrPresVal(iYear), 2);
                totalPV += CashFlow(costCatTotGrand).yrPresVal(iYear);
            }

            rowHead(state.dataEconLifeCycleCost->lengthStudyYears + 1) = "TOTAL";
            tableBody(3, state.dataEconLifeCycleCost->lengthStudyYears + 1) = RealToStr(totalPV, 2);

            WriteSubtitle(state, "Present Value by Year");
            WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
            if (sqlite) {
                sqlite->createSQLiteTabularDataRecords(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Year");
            }
            if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
                    tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Year");
            }
            columnHead.deallocate();
            rowHead.deallocate();
            columnWidth.deallocate();
            tableBody.deallocate();
            //---- After Tax Estimate
            if (state.dataEconLifeCycleCost->taxRate != 0.0) {
                rowHead.allocate(state.dataEconLifeCycleCost->lengthStudyYears + 1);
                columnHead.allocate(5);
                columnWidth.allocate(5);
                columnWidth = 14; // array assignment - same for all columns
                tableBody.allocate(5, state.dataEconLifeCycleCost->lengthStudyYears + 1);
                tableBody = "";
                columnHead(1) = "Depreciated Capital";
                columnHead(2) = "Taxable Income";
                columnHead(3) = "Income Taxes";
                columnHead(4) = "After Tax Cash Flow";
                columnHead(5) = "After Tax Present Value";

                totalPV = 0.0;
                for (iYear = 1; iYear <= state.dataEconLifeCycleCost->lengthStudyYears; ++iYear) {
                    rowHead(iYear) = format("{} {}", MonthNames(state.dataEconLifeCycleCost->baseDateMonth), state.dataEconLifeCycleCost->baseDateYear + iYear - 1);
                    tableBody(1, iYear) = RealToStr(DepreciatedCapital(iYear), 2);
                    tableBody(2, iYear) = RealToStr(TaxableIncome(iYear), 2);
                    tableBody(3, iYear) = RealToStr(Taxes(iYear), 2);
                    tableBody(4, iYear) = RealToStr(AfterTaxCashFlow(iYear), 2);
                    tableBody(5, iYear) = RealToStr(AfterTaxPresentValue(iYear), 2);
                    totalPV += AfterTaxPresentValue(iYear);
                }

                rowHead(state.dataEconLifeCycleCost->lengthStudyYears + 1) = "TOTAL";
                tableBody(5, state.dataEconLifeCycleCost->lengthStudyYears + 1) = RealToStr(totalPV, 2);

                WriteSubtitle(state, "After Tax Estimate");
                WriteTable(state, tableBody, rowHead, columnHead, columnWidth);
                if (sqlite) {
                    sqlite->createSQLiteTabularDataRecords(
                        tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "After Tax Estimate");
                }
                if (ResultsFramework::resultsFramework->timeSeriesAndTabularEnabled()) {
                    ResultsFramework::resultsFramework->TabularReportsCollection.addReportTable(
                        tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "After Tax Estimate");
                }
                columnHead.deallocate();
                rowHead.deallocate();
                columnWidth.deallocate();
                tableBody.deallocate();
            }
        }
    }

    void clear_state()
    {
        SPV.deallocate();
        DepreciatedCapital.deallocate();
        TaxableIncome.deallocate();
        Taxes.deallocate();
        AfterTaxCashFlow.deallocate();
        AfterTaxPresentValue.deallocate();
        RecurringCosts.deallocate();
        NonrecurringCost.deallocate();
        UsePriceEscalation.deallocate();
        UseAdjustment.deallocate();
        CashFlow.deallocate();

        GetInput_GetLifeCycleCostInput = true;
        UsePriceEscalation_escStartYear = 0;
        UsePriceEscalation_escNumYears = 0;
        UsePriceEscalation_escEndYear = 0;
        UsePriceEscalation_earlierEndYear = 0;
        UsePriceEscalation_laterStartYear = 0;
        UsePriceEscalation_curEsc = 0;
        UsePriceEscalation_curFld = 0;
        ExpressAsCashFlows_baseMonths1900 = 0;
        ExpressAsCashFlows_serviceMonths1900 = 0;
    }

} // namespace EnergyPlus
