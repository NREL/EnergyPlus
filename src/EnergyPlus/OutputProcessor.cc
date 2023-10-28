// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_set>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include "re2/re2.h"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ResultsFramework.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include <fmt/ostream.h>
#include <milo/dtoa.h>
#include <milo/itoa.h>

namespace EnergyPlus {

namespace OutputProcessor {

    // MODULE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   December 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module contains the major Output Processor routines.
    // In addition, in this file are several routines which can be called
    // without using the OutputProcessor Module

    // METHODOLOGY EMPLOYED:
    // Lots of pointers and other fancy data stuff.  (I didn't see a single pointer here)

    // Routines tagged on the end of this module:
    //  AddDDOutVar
    //  GenOutputVariablesAuditReport
    //  GetCurrentMeterValue
    //  GetInstantMeterValue
    //  GetInternalVariableValue
    //  GetInternalVariableValueExternalInterface
    //  GetMeteredVariables
    //  GetMeterIndex
    //  GetMeterResourceType
    //  GetNumMeteredVariables
    //  GetVariableKeyCountandType
    //  GetVariableKeys
    //  InitPollutionMeterReporting
    //  ProduceRDDMDD
    //  ReportingThisVariable
    //  SetInitialMeterReportingAndOutputNames
    //  SetupOutputVariable
    //  UpdateDataandReport
    //  UpdateMeterReporting

    // Functions

    int DetermineMinuteForReporting(EnergyPlusData &state)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // When reporting peaks, minutes are used but not necessarily easily calculated.

        Real64 constexpr FracToMin(60.0);
        return ((state.dataGlobal->CurrentTime + state.dataHVACGlobal->SysTimeElapsed) - int(state.dataGlobal->CurrentTime)) * FracToMin;
    }

    void InitializeOutput(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes the OutputProcessor data structures.

        auto &op = state.dataOutputProcessor;

        // Initialize end use category names - the indices must match up with endUseNames in OutputReportTabular
        op->EndUseCategory.allocate((int)Constant::EndUse::Num);
        op->EndUseCategory((int)Constant::EndUse::Heating + 1).Name = "Heating";
        op->EndUseCategory((int)Constant::EndUse::Cooling + 1).Name = "Cooling";
        op->EndUseCategory((int)Constant::EndUse::InteriorLights + 1).Name = "InteriorLights";
        op->EndUseCategory((int)Constant::EndUse::ExteriorLights + 1).Name = "ExteriorLights";
        op->EndUseCategory((int)Constant::EndUse::InteriorEquipment + 1).Name = "InteriorEquipment";
        op->EndUseCategory((int)Constant::EndUse::ExteriorEquipment + 1).Name = "ExteriorEquipment";
        op->EndUseCategory((int)Constant::EndUse::Fans + 1).Name = "Fans";
        op->EndUseCategory((int)Constant::EndUse::Pumps + 1).Name = "Pumps";
        op->EndUseCategory((int)Constant::EndUse::HeatRejection + 1).Name = "HeatRejection";
        op->EndUseCategory((int)Constant::EndUse::Humidification + 1).Name = "Humidifier";
        op->EndUseCategory((int)Constant::EndUse::HeatRecovery + 1).Name = "HeatRecovery";
        op->EndUseCategory((int)Constant::EndUse::WaterSystem + 1).Name = "WaterSystems";
        op->EndUseCategory((int)Constant::EndUse::Refrigeration + 1).Name = "Refrigeration";
        op->EndUseCategory((int)Constant::EndUse::Cogeneration + 1).Name = "Cogeneration";
        
        // Initialize display names for output table - this could go away if end use key names are changed to match
        op->EndUseCategory((int)Constant::EndUse::Heating + 1).DisplayName = "Heating";
        op->EndUseCategory((int)Constant::EndUse::Cooling + 1).DisplayName = "Cooling";
        op->EndUseCategory((int)Constant::EndUse::InteriorLights + 1).DisplayName = "Interior Lighting";
        op->EndUseCategory((int)Constant::EndUse::ExteriorLights + 1).DisplayName = "Exterior Lighting";
        op->EndUseCategory((int)Constant::EndUse::InteriorEquipment + 1).DisplayName = "Interior Equipment";
        op->EndUseCategory((int)Constant::EndUse::ExteriorEquipment + 1).DisplayName = "Exterior Equipment";
        op->EndUseCategory((int)Constant::EndUse::Fans + 1).DisplayName = "Fans";
        op->EndUseCategory((int)Constant::EndUse::Pumps + 1).DisplayName = "Pumps";
        op->EndUseCategory((int)Constant::EndUse::HeatRejection + 1).DisplayName = "Heat Rejection";
        op->EndUseCategory((int)Constant::EndUse::Humidification + 1).DisplayName = "Humidification";
        op->EndUseCategory((int)Constant::EndUse::HeatRecovery + 1).DisplayName = "Heat Recovery";
        op->EndUseCategory((int)Constant::EndUse::WaterSystem + 1).DisplayName = "Water Systems";
        op->EndUseCategory((int)Constant::EndUse::Refrigeration + 1).DisplayName = "Refrigeration";
        op->EndUseCategory((int)Constant::EndUse::Cogeneration + 1).DisplayName = "Generators";
        
        op->OutputInitialized = true;

        op->TimeStepZoneSec = double(state.dataGlobal->MinutesPerTimeStep) * 60.0;

        state.files.mtd.ensure_open(state, "InitializeMeters", state.files.outputControl.mtd);
    } // InitializeOutput()

    void addEndUseSubcategory(EnergyPlusData &state, std::string_view const endUseName, std::string_view const endUseSubName)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   February 2006

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages the list of subcategories for each end-use category.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &op = state.dataOutputProcessor;
        
        bool Found = false;
        for (int EndUseNum = 1; EndUseNum <= (int)Constant::EndUse::Num; ++EndUseNum) {
                auto &endUse = op->EndUseCategory(EndUseNum);
            if (UtilityRoutines::SameString(endUse.Name, endUseName)) {
                for (int EndUseSubNum = 1; EndUseSubNum <= op->EndUseCategory(EndUseNum).NumSubcategories; ++EndUseSubNum) {
                    if (UtilityRoutines::SameString(endUse.SubcategoryName(EndUseSubNum), endUseSubName)) {
                        // Subcategory already exists, no further action required
                        Found = true;
                        break;
                    }
                }
                
                if (!Found) {
                    // Add the subcategory by reallocating the array
                    endUse.SubcategoryName.redimension(++endUse.NumSubcategories);
                    endUse.SubcategoryName(endUse.NumSubcategories) = endUseSubName;
                    
                    if (endUse.NumSubcategories > op->MaxNumSubcategories) {
                        op->MaxNumSubcategories = endUse.NumSubcategories;
                    }
                    
                    Found = true;
                }
                break;
            }
        }

        if (!Found) {
            ShowSevereError(state, format("Nonexistent end use passed to AddEndUseSubcategory={}", endUseName));
        }
    } // addEndUseSubcategory()
        
    void addEndUseSpaceType(EnergyPlusData &state, std::string_view const EndUseName, std::string_view const EndUseSpaceTypeName)
    {
        auto &op = state.dataOutputProcessor;
        
        bool Found = false;
        for (size_t EndUseNum = 1; EndUseNum <= static_cast<size_t>(Constant::EndUse::Num); ++EndUseNum) {
            if (UtilityRoutines::SameString(op->EndUseCategory(EndUseNum).Name, EndUseName)) {
                for (int endUseSpTypeNum = 1; endUseSpTypeNum <= op->EndUseCategory(EndUseNum).numSpaceTypes; ++endUseSpTypeNum) {
                    if (UtilityRoutines::SameString(op->EndUseCategory(EndUseNum).spaceTypeName(endUseSpTypeNum), EndUseSpaceTypeName)) {
                        // Space type already exists, no further action required
                        Found = true;
                        break;
                    }
                }
                
                if (!Found) {
                   // Add the space type by reallocating the array
                   int numSpTypes = op->EndUseCategory(EndUseNum).numSpaceTypes;
                   op->EndUseCategory(EndUseNum).spaceTypeName.redimension(numSpTypes + 1);
                
                   op->EndUseCategory(EndUseNum).numSpaceTypes = numSpTypes + 1;
                   op->EndUseCategory(EndUseNum).spaceTypeName(numSpTypes + 1) = EndUseSpaceTypeName;
                
                   if (op->EndUseCategory(EndUseNum).numSpaceTypes > op->maxNumEndUseSpaceTypes) {
                       op->maxNumEndUseSpaceTypes = op->EndUseCategory(EndUseNum).numSpaceTypes;
                   }
                   
                   Found = true;
                }
                break;
            }
        }
        
        if (!Found) {
            ShowSevereError(state, format("Nonexistent end use passed to addEndUseSpaceType={}", EndUseName));
        }
    } // addEndUseSpaceType()
        
    void SetupTimePointers(EnergyPlusData &state,
                           SOVTimeStepType const sovTimeStep, // Which timestep is being set up, 'Zone'=1, 'HVAC'=2
                           Real64 &TimeStep                                        // The timestep variable.  Used to get the address
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets up the derived type for the output processor that
        // contains pointers to the TimeStep values used in the simulation.

        // METHODOLOGY EMPLOYED:
        // Indicate that the TimeStep passed in is a target for the pointer
        // attributes in the derived types.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // ValidateTimeStepType will throw a Fatal if not valid
        TimeStepType timeStep = sovTimeStep2TimeStep[(int)sovTimeStep];

        if (state.dataOutputProcessor->TimeValue[(int)timeStep].TimeStep != nullptr) {
            ShowFatalError(state, format("SetupTimePointers was already called for {}", sovTimeStepTypeStrings[(int)sovTimeStep]));
        }
        state.dataOutputProcessor->TimeValue[(int)timeStep].TimeStep = &TimeStep;
    }

    void CheckReportVariable(EnergyPlusData &state,
                             std::string_view const Name,
                             std::string const &Key,
                             std::vector<int> &reqVarList)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine will get the report variable information from input and
        // determine if this variable (KeyedValue and VariableName) should be reported
        // and, if so, what frequency to report.

        // This routine is called when SetupOutputVariable is called with no "optional"
        // Reporting Frequency.  It is expected that SetupOutputVariable would only be
        // called once for each keyed variable to be triggered for output (from the input
        // requests).  The optional report frequency would only be used for debugging
        // purposes.  Therefore, this routine will collect all occasions where this
        // passed variablename would be reported from the requested input.  It builds
        // a list of these requests (ReportList) so that the calling routine can propagate
        // the requests into the correct data structure.

        // METHODOLOGY EMPLOYED:
        // This instance being requested will always have a key associated with it.  Matching
        // instances (from input) may or may not have keys, but only one instance of a reporting
        // frequency per variable is allowed.  ReportList will be populated with ReqRepVars indices
        // of those extra things from input that satisfy this condition.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Make sure that input has been read
        GetReportVariableInput(state);

        auto &op = state.dataOutputProcessor;

        for (int iReqVar = 0; iReqVar < (int)op->reqVars.size(); ++iReqVar) {
            auto *reqVar = op->reqVars[iReqVar];

            if (!UtilityRoutines::SameString(reqVar->name, Name)) {
                continue;
            }

            if (!reqVar->key.empty() && !(reqVar->is_simple_string && UtilityRoutines::SameString(reqVar->key, Key)) &&
                !(!reqVar->is_simple_string && RE2::FullMatch(std::string{Key}, *(reqVar->case_insensitive_pattern)))) {
                continue;
            }

            // A match. Make sure doesn't duplicate
            reqVar->Used = true;
            bool Dup = false;
            // op->ReportList is allocated to a large value, so we can't use a std::find_if on it (why not?)
            for (int iReqVar2 : reqVarList) { 
                if (op->reqVars[iReqVar2]->freq == reqVar->freq && op->reqVars[iReqVar2]->SchedPtr == reqVar->SchedPtr) {
                    Dup = true;
                    break;
                }
            }

            if (!Dup) {
                reqVarList.push_back(iReqVar);
            }
        }
    }

    constexpr std::array<std::string_view, (int)ReportFreq::Num> reportingFrequencyNoticeStrings = {
        " !Each Call", // EachCall
        " !TimeStep", // TimeStep
        " !Hourly", // Hourly
        " !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]", // Daily
        " !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]", // Monthly
        " !Annual [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]", // Yearly
        " !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" // Simulation
    };

    ReportFreq determineFrequency(EnergyPlusData &state, const std::string_view FreqString)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       December 2017; Jason DeGraw

        // REFERENCES:
        //       \field Reporting Frequency
        //       \type choice
        //       \key Detailed
        //       \note Detailed lists every instance (i.e. HVAC variable timesteps)
        //       \key Timestep
        //       \note Timestep refers to the zone Timestep/Number of Timesteps in hour value
        //       \note RunPeriod, Environment, and Annual are the same
        //       \key Hourly
        //       \key Daily
        //       \key Monthly
        //       \key RunPeriod
        //       \key Environment
        //       \key Annual
        //       \default Hourly
        //       \note RunPeriod and Environment are synonymous

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::array<std::string_view, (int)ReportFreq::Num+1> PossibleFreqs =
            {"DETA", "TIME", "HOUR", "DAIL", "MONT", "RUNP", "ENVI", "ANNU"};
        //=(/'detail','Timestep','Hourly','Daily','Monthly','RunPeriod','Environment','Annual'/)
        static constexpr std::array<std::string_view, (int)ReportFreq::Num+1> ExactFreqStrings =
            {"Detailed", "Timestep", "Hourly", "Daily", "Monthly", "RunPeriod", "Environment", "Annual"};
        static constexpr std::array<std::string_view, (int)ReportFreq::Num+1>  ExactFreqStringsUC =
            {"DETAILED", "TIMESTEP", "HOURLY", "DAILY", "MONTHLY", "RUNPERIOD", "ENVIRONMENT", "ANNUAL"};
        // Vector of the result, was { -1, 0, 1, 2, 3, 4, 4, 4 } before the addition of Yearly;
        static constexpr std::array<ReportFreq, (int)ReportFreq::Num+1> FreqValues =
            {ReportFreq::EachCall, ReportFreq::TimeStep, ReportFreq::Hour, ReportFreq::Day,
             ReportFreq::Month, ReportFreq::Simulation, ReportFreq::Simulation, ReportFreq::Year};

        ReportFreq freq = ReportFreq::Hour; // Default
        // TODO: I think it's supposed to be upper case already, but tests aren't doing that at least...
        const std::string FreqStringUpper = UtilityRoutines::MakeUPPERCase(FreqString);
        std::string::size_type const LenString = min(len(FreqString), static_cast<std::string::size_type>(4u));

        if (LenString < 4u) {
            return freq;
        }

        std::string const FreqStringTrim(FreqStringUpper.substr(0, LenString));
        for (unsigned Loop = 0; Loop < FreqValues.size(); ++Loop) {
            if (FreqStringTrim == PossibleFreqs[Loop]) {
                if (FreqStringUpper != ExactFreqStringsUC[Loop]) {
                    ShowWarningError(state, format("DetermineFrequency: Entered frequency=\"{}\" is not an exact match to key strings.", FreqString));
                    ShowContinueError(state, format("Frequency={} will be used.", ExactFreqStrings[Loop]));
                }
                freq = std::max(FreqValues[Loop], state.dataOutputProcessor->minimumReportFreq);
                break;
            }
        }
        return freq;
    }

    void GetReportVariableInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       December 2017; Jason DeGraw

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the requested report variables from
        // the input file.
        // Report Variable,
        //        \memo each Report Variable command picks variables to be put onto the standard output file (.eso)
        //        \memo some variables may not be reported for every simulation
        //   A1 , \field Key_Value
        //        \note use '*' (without quotes) to apply this variable to all keys
        //   A2 , \field Variable_Name
        //   A3 , \field Reporting_Frequency
        //        \type choice
        //        \key detailed
        //        \key timestep
        //        \key hourly
        //        \key daily
        //        \key monthly
        //        \key runperiod
        //   A4 ; \field Schedule_Name
        //        \type object-list
        //        \object-list ScheduleNames

        constexpr std::string_view routineName = "GetReportVariableInput";
        // Using/Aliasing
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;
        int NumNumbers;
        int IOStat;
        bool ErrorsFound(false); // If errors detected in input
        std::string cCurrentModuleObject;
        Array1D_string cAlphaArgs(4);
        Array1D_string cAlphaFieldNames(4);
        Array1D_bool lAlphaBlanks(4);
        Array1D<Real64> rNumericArgs(1);
        Array1D_string cNumericFieldNames(1);
        Array1D_bool lNumericBlanks(1);
        auto &op = state.dataOutputProcessor;

        // Bail out if the input has already been read in
        if (!op->GetOutputInputFlag) {
            return;
        }
        op->GetOutputInputFlag = false;

        // First check environment variable to see of possible override for minimum reporting frequency
        if (!state.dataSysVars->MinReportFrequency.empty()) {
            // Formats
            static constexpr std::string_view Format_800("! <Minimum Reporting Frequency (overriding input value)>, Value, Input Value\n");
            static constexpr std::string_view Format_801(" Minimum Reporting Frequency, {},{}\n");
            op->minimumReportFreq = determineFrequency(state, state.dataSysVars->MinReportFrequency);
            print(state.files.eio, Format_800);
            print(state.files.eio, Format_801, reportingFrequencyNoticeStrings[(int)op->minimumReportFreq], state.dataSysVars->MinReportFrequency);
        }

        cCurrentModuleObject = "Output:Variable";
        op->NumOfReqVariables = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        op->reqVars.reserve(op->NumOfReqVariables);

        for (int Loop = 1; Loop <= op->NumOfReqVariables; ++Loop) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Loop,
                                                                     cAlphaArgs,
                                                                     NumAlpha,
                                                                     rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);


            ErrorObjectHeader eoh{routineName, cCurrentModuleObject, cAlphaArgs(1)};
            
            // Check for duplicates?
            ReqVar *reqVar = new ReqVar();
            op->reqVars.push_back(reqVar);
            
            reqVar->key = cAlphaArgs(1);
            if (reqVar->key == "*") {
                reqVar->key = std::string();
            }

            bool is_simple_string = !DataOutputs::isKeyRegexLike(reqVar->key);
            reqVar->is_simple_string = is_simple_string;
            if (!is_simple_string) {
                reqVar->case_insensitive_pattern = std::make_shared<RE2>("(?i)" + reqVar->key);
            }

            std::string::size_type const lbpos = index(cAlphaArgs(2), '['); // Remove Units designation if user put it in
            if (lbpos != std::string::npos) {
                cAlphaArgs(2).erase(lbpos);
                // right trim
                cAlphaArgs(2) = cAlphaArgs(2).substr(0, std::min(cAlphaArgs(2).find_last_not_of(" \f\n\r\t\v") + 1, cAlphaArgs(2).size()));
            }
            reqVar->name = cAlphaArgs(2);

            reqVar->freq = determineFrequency(state, UtilityRoutines::MakeUPPERCase(cAlphaArgs(3)));
            if (reqVar->freq == ReportFreq::Invalid) {
                ShowSevereInvalidKey(state, eoh, cAlphaFieldNames(3), cAlphaArgs(3));
                ErrorsFound = true;
            }

            // Schedule information
            if (lAlphaBlanks(4)) {
                reqVar->SchedPtr = 0;
            } else if ((reqVar->SchedPtr = GetScheduleIndex(state, UtilityRoutines::MakeUPPERCase(cAlphaArgs(4)))) == 0) { 
                ShowSevereItemNotFound(state, eoh, cAlphaFieldNames(4), cAlphaArgs(4));
                ErrorsFound = true;
            }

            reqVar->Used = false;
        }

        if (ErrorsFound) {
            ShowFatalError(state, format("GetReportVariableInput:{}: errors in input.", cCurrentModuleObject));
        }
    }

    void ProduceMinMaxString(std::string &String,                // Current value
                             int const DateValue,                // Date of min/max
                             ReportFreq const ReportFreq // Reporting Frequency
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine produces the appropriate min/max string depending
        // on the reporting frequency.

        // METHODOLOGY EMPLOYED:
        // Prior to calling this routine, the basic value string will be
        // produced, but DecodeMonDayHrMin will not have been called.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view DayFormat("{},{:2},{:2}");
        static constexpr std::string_view MonthFormat("{},{:2},{:2},{:2}");
        static constexpr std::string_view EnvrnFormat("{},{:2},{:2},{:2},{:2}");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Mon;
        int Day;
        int Hour;
        int Minute;
        General::DecodeMonDayHrMin(DateValue, Mon, Day, Hour, Minute);

        switch (ReportFreq) {
        case ReportFreq::Day:
            String = format(DayFormat, strip(String), Hour, Minute);
            return;
        case ReportFreq::Month:
            String = format(MonthFormat, strip(String), Day, Hour, Minute);
            return;
        case ReportFreq::Year:
        case ReportFreq::Simulation:
            String = format(EnvrnFormat, strip(String), Mon, Day, Hour, Minute);
            return;
        default: // Each, TimeStep, Hourly dont have this
            String = std::string();
            return;
        }
    }

    // *****************************************************************************
    // The following routines implement Energy Meters in EnergyPlus.
    // *****************************************************************************

    void GetCustomMeterInput(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2006

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will help implement "custom"/user defined meters.  However, it must be called after all
        // the other meters are set up and all report variables are established.

        // REFERENCES:
        // Processes the objects:
        // Meter:Custom,
        //    \extensible:2 - repeat last two fields, remembering to remove ; from "inner" fields.
        //    \memo Used to allow users to combine specific variables and/or meters into
        //    \memo "custom" meter configurations.
        //    A1,  \field Name
        //         \required-field
        //         \reference CustomMeterNames
        //    A2,  \field Fuel Type
        //         \type choice
        //         \key Electricity
        //         \key NaturalGas
        //         \key PropaneGas
        //         \key FuelOilNo1
        //         \key FuelOilNo2
        //         \key Coal
        //         \key Diesel
        //         \key Gasoline
        //         \key Water
        //         \key Generic
        //         \key OtherFuel1
        //         \key OtherFuel2
        //    A3,  \field Key Name 1
        //         \required-field
        //         \begin-extensible
        //    A4,  \field Report Variable or Meter Name 1
        //         \required-field
        // <etc>
        // AND
        // Meter:CustomDecrement,
        //    \extensible:2 - repeat last two fields, remembering to remove ; from "inner" fields.
        //    \memo Used to allow users to combine specific variables and/or meters into
        //    \memo "custom" meter configurations.
        //    A1,  \field Name
        //         \required-field
        //         \reference CustomMeterNames
        //    A2,  \field Fuel Type
        //         \type choice
        //         \key Electricity
        //         \key NaturalGas
        //         \key PropaneGas
        //         \key FuelOilNo1
        //         \key FuelOilNo2
        //         \key Coal
        //         \key Diesel
        //         \key Gasoline
        //         \key Water
        //         \key Generic
        //         \key OtherFuel1
        //         \key OtherFuel2
        //    A3,  \field Source Meter Name
        //         \required-field
        //    A4,  \field Key Name 1
        //         \required-field
        //         \begin-extensible
        //    A5,  \field Report Variable or Meter Name 1
        //         \required-field
        // <etc>

        constexpr std::string_view routineName = "GetCustomMeterInput";
            
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &op = state.dataOutputProcessor;
        auto &ip = state.dataInputProcessing->inputProcessor;
        auto &ipsc = state.dataIPShortCut;
        
        int NumAlpha;
        int NumNumbers;
        int IOStat;
        Array1D_string NamesOfKeys;                                    // Specific key name
        Array1D_int IndexesForKeyVar;                                  // Array index

        std::vector<int> onCustomMeterVarNums;
        Array1D_int VarsOnSourceMeter;
        bool errFlag;
        bool BigErrorsFound;

        BigErrorsFound = false;

        int numCustomMeters = 0, numCustomDecMeters = 0;
        std::vector<std::string> customMeterNames;
        if (auto const found = ip->epJSON.find("Meter:Custom"); found != ip->epJSON.end()) {
            for (auto meterInstance = found.value().begin(); meterInstance != found.value().end(); ++meterInstance, ++numCustomMeters)
                customMeterNames.push_back(UtilityRoutines::MakeUPPERCase(meterInstance.key()));
        }

        if (auto const found = ip->epJSON.find("Meter:CustomDecrement"); found != ip->epJSON.end()) {
            for (auto meterInstance = found.value().begin(); meterInstance != found.value().end(); ++meterInstance, ++numCustomDecMeters)
                customMeterNames.push_back(UtilityRoutines::MakeUPPERCase(meterInstance.key()));
        }

        ipsc->cCurrentModuleObject = "Meter:Custom";
        for (int Loop = 1; Loop <= numCustomMeters; ++Loop) {
            ip->getObjectItem(state,
                              ipsc->cCurrentModuleObject,
                              Loop,
                              ipsc->cAlphaArgs,
                              NumAlpha,
                              ipsc->rNumericArgs,
                              NumNumbers,
                              IOStat,
                              ipsc->lNumericFieldBlanks,
                              ipsc->lAlphaFieldBlanks,
                              ipsc->cAlphaFieldNames,
                              ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};

            std::string meterNameUC = UtilityRoutines::MakeUPPERCase(ipsc->cAlphaArgs(1));
            std::string::size_type lbrackPos = index(meterNameUC, '[');
            if (lbrackPos != std::string::npos) meterNameUC.erase(lbrackPos);

            if (op->meterMap.find(meterNameUC) != op->meterMap.end()) {
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }

            int meterNum = AddCustomMeter(state, ipsc->cAlphaArgs(1));
            auto *meter = op->meters[meterNum];
            meter->type = MeterType::Custom;
            // Can't use resource type in AddMeter cause it will confuse it with other meters.  So, now:
            meter->resource = static_cast<Constant::eResource>(getEnumerationValue(Constant::eResourceNamesUC,
                                                                                   UtilityRoutines::MakeUPPERCase(ipsc->cAlphaArgs(2))));
            if (meter->resource == Constant::eResource::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                BigErrorsFound = true;
            }

            for (int fldIndex = 3; fldIndex <= NumAlpha; fldIndex += 2) {
                bool KeyIsStar = (ipsc->cAlphaArgs(fldIndex) == "*" || ipsc->lAlphaFieldBlanks(fldIndex));

                if (ipsc->lAlphaFieldBlanks(fldIndex + 1)) {
                    ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(fldIndex + 1));
                    ShowContinueError(state, "...cannot create custom meter.");
                    BigErrorsFound = true;
                    continue;
                }

                std::string meterOrVarName = ipsc->cAlphaArgs(fldIndex + 1);
                
                // Don't build/check things out if there were errors anywhere.  Use "GetVariableKeys" to map to actual variables...
                std::string::size_type lbrackPos = index(meterOrVarName, '[');
                if (lbrackPos != std::string::npos) meterOrVarName.erase(lbrackPos);

                // A custom meter cannot reference another custom meter
                if (std::find(customMeterNames.begin(), customMeterNames.end(), meterOrVarName) != customMeterNames.end()) {
                    ShowWarningCustomMessage(state, eoh, format("{}=\"{}\" is a reference to another {}.",
                                                                ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName, ipsc->cCurrentModuleObject));
                    continue;
                }

                // This is a meter
                if (auto foundSrcMeter = op->meterMap.find(meterOrVarName); foundSrcMeter != op->meterMap.end()) {
                    int srcMeterNum = foundSrcMeter->second;
                    auto *srcMeter = op->meters[srcMeterNum];
                    assert(srcMeter->type == MeterType::Normal);

                    // If it's the first meter, it gets to set the units
                    if (meter->units == Constant::Units::Invalid) {
                        meter->units = srcMeter->units;
                        meter->RT_forIPUnits = GetResourceIPUnits(state, meter->resource, meter->units, errFlag);
                        if (errFlag) {
                            ShowContinueError(state, format("..on {}=\"{}\".", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                            ShowContinueError(state, "..requests for IP units from this meter will be ignored.");
                        }

                    } else if (meter->units != srcMeter->units) {
                        ShowWarningCustomMessage(state, eoh, format("{}=\"{}\", differing units in {}=\"{}\".",
                                                                    ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName));
                        ShowContinueError(state, format("...will not be shown with the Meter results; units for meter={}, units for this variable={}.",
                                                        Constant::unitNames[(int)meter->units], Constant::unitNames[(int)srcMeter->units]));
                        continue;
                    }

                    // Check for duplicates
                    if (std::find(meter->srcMeterNums.begin(), meter->srcMeterNums.end(), srcMeterNum) != meter->srcMeterNums.end()) {
                        ShowWarningCustomMessage(state, eoh, format("{}=\"{}\" referenced multiple times, only first instance will be used",
                                                                    ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName));
                        continue;
                    }

                    // Link meter to src meter and var and vice versa
                    meter->srcMeterNums.push_back(srcMeterNum);
                    srcMeter->dstMeterNums.push_back(meterNum);
                    meter->srcVarNums.push_back(srcMeter->outVarNum);
                    op->outVars[srcMeter->outVarNum]->meterNums.push_back(meterNum);

                // This is a variable
                } else if (auto foundSrcDDVar = op->ddOutVarMap.find(meterOrVarName); foundSrcDDVar != op->ddOutVarMap.end()) {
                    int srcDDVarNum = foundSrcDDVar->second;
                    auto *srcDDVar = op->ddOutVars[srcDDVarNum];

                    // Has to be a summed variable
                    if (srcDDVar->storeType != StoreType::Summed) {
                        ShowWarningCustomMessage(state, eoh, format("{}=\"{}\", variable not summed variable {}=\"{}\".",
                                                                    ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName));
                        ShowContinueError(state, format("...will not be shown with the Meter results; units for meter={}, units for this variable={}.",
                                                        Constant::unitNames[(int)meter->units], Constant::unitNames[(int)srcDDVar->units]));
                        continue;
                    }
                    
                    // If it's the first variable, it gets to set the units
                    if (meter->units == Constant::Units::Invalid) {
                        meter->units = srcDDVar->units;
                        meter->RT_forIPUnits = GetResourceIPUnits(state, meter->resource, meter->units, errFlag);
                        if (errFlag) {
                            ShowContinueError(state, format("..on {}=\"{}\".", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                            ShowContinueError(state, "..requests for IP units from this meter will be ignored.");
                        }

                    // Otherwise it has to match the existing units
                    } else if (meter->units != srcDDVar->units) {
                        ShowWarningCustomMessage(state, eoh, format("differing units in {}=\"{}\".",
                                                                    ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName));
                        ShowContinueError(state, format("...will not be shown with the Meter results; units for meter={}, units for this variable={}.",
                                                        Constant::unitNames[(int)meter->units], Constant::unitNames[(int)srcDDVar->units]));
                        continue;
                    }
                    
                    if (KeyIsStar) {
                        if (srcDDVar->keyOutVarNums.size() > 0) {
                            for (int keyOutVarNum : srcDDVar->keyOutVarNums) {
                                if (std::find(meter->srcVarNums.begin(), meter->srcVarNums.end(), keyOutVarNum) != meter->srcVarNums.end()) {
                                    ShowWarningCustomMessage(state, eoh, format("Output variable \"{}\" referenced multiple times (directly or via meter)", 
                                                                                op->outVars[keyOutVarNum]->keyColonNameUC));
                                        
                                } else {
                                    meter->srcVarNums.push_back(keyOutVarNum);
                                    op->outVars[keyOutVarNum]->meterNums.push_back(meterNum);
                                }
                            }
                        } else {
                            ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName);
                            ErrorsFound = true;
                        }
                    } else { // Key is not "*"
                        bool foundKey = false;
                        for (int keyOutVarNum : srcDDVar->keyOutVarNums) {
                            if (op->outVars[keyOutVarNum]->keyUC == ipsc->cAlphaArgs(fldIndex)) {
                                if (std::find(meter->srcVarNums.begin(), meter->srcVarNums.end(), keyOutVarNum) != meter->srcVarNums.end()) {
                                    ShowWarningCustomMessage(state, eoh, format("Output variable \"{}\" referenced multiple times (directly or via meter)", 
                                                                                op->outVars[keyOutVarNum]->keyColonNameUC));
                                } else {
                                    meter->srcVarNums.push_back(keyOutVarNum);
                                    op->outVars[keyOutVarNum]->meterNums.push_back(meterNum);
                                }
                                foundKey = true;
                                break;
                            }
                        }
                        if (!foundKey) {
                            ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName);
                            ErrorsFound = true;
                        }
                    }

                // Neither a meter nor a variable    
                } else {                        
                    // Should this only be a warning? If I had a custom meter and one of its components was
                    // wrong, I would want the entire custom meter to "fail", rather than for that component
                    // to be ommitted with only a warning.
                    ShowWarningItemNotFound(state, eoh, ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName);
                    ShowContinueError(state, "...will not be shown with the Meter results.");
                    continue;
                }                    
                
            } // for (fldIndex)

            // Somehow, this meter is not linked to any variables either directly or via another meter
            if (meter->srcVarNums.size() == 0) {
                ShowWarningCustomMessage(state, eoh, "no items assigned ");
                ShowContinueError(state, "...will not be shown with Meter results. This may be caused by a Meter:Custom assigned to another Meter:Custom.");
            }
        } // for (Loop)

        ipsc->cCurrentModuleObject = "Meter:CustomDecrement";
        for (int Loop = 1; Loop <= numCustomDecMeters; ++Loop) {
            ip->getObjectItem(state,
                              ipsc->cCurrentModuleObject,
                              Loop,
                              ipsc->cAlphaArgs,
                              NumAlpha,
                              ipsc->rNumericArgs,
                              NumNumbers,
                              IOStat,
                              ipsc->lNumericFieldBlanks,
                              ipsc->lAlphaFieldBlanks,
                              ipsc->cAlphaFieldNames,
                              ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            std::string meterNameUC = UtilityRoutines::MakeUPPERCase(ipsc->cAlphaArgs(1));
            std::string::size_type lbrackPos = index(meterNameUC, '[');
            if (lbrackPos != std::string::npos) meterNameUC.erase(lbrackPos);

            if (op->meterMap.find(meterNameUC) != op->meterMap.end()) {
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }

            int meterNum = AddCustomMeter(state, ipsc->cAlphaArgs(1));
            auto *meter = op->meters[meterNum];
            meter->type = MeterType::CustomDec;
            // Can't use resource type in AddMeter cause it will confuse it with other meters.  So, now:
            meter->resource = static_cast<Constant::eResource>(getEnumerationValue(Constant::eResourceNamesUC,
                                                                                   UtilityRoutines::MakeUPPERCase(ipsc->cAlphaArgs(2))));
            if (meter->resource == Constant::eResource::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                BigErrorsFound = true;
            }

            std::string decMeterName = UtilityRoutines::MakeUPPERCase(ipsc->cAlphaArgs(3));
            lbrackPos = index(decMeterName, '[');
            if (lbrackPos != std::string::npos) decMeterName.erase(lbrackPos);
            
            auto foundDecMeter = op->meterMap.find(decMeterName);
            if (foundDecMeter == op->meterMap.end()) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), decMeterName);
                ErrorsFound = true;
                continue;
            }
        
            meter->decMeterNum = foundDecMeter->second;
            auto *decMeter = op->meters[meter->decMeterNum];
            assert(decMeter->type == MeterType::Normal);

            meter->units = decMeter->units;
            meter->RT_forIPUnits = GetResourceIPUnits(state, meter->resource, meter->units, errFlag);
            if (errFlag) {
                ShowContinueError(state, format("..on {}=\"{}\".", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ShowContinueError(state, "..requests for IP units from this meter will be ignored.");
            }

            //  Links meter to dec meter and its output variable and vice versa
            meter->srcMeterNums.push_back(meter->decMeterNum);
            decMeter->dstMeterNums.push_back(meterNum);
            
            int decOutVarNum = decMeter->outVarNum;
            auto *decOutVar = op->outVars[decOutVarNum];
            
            meter->srcVarNums.push_back(decOutVarNum);
            decOutVar->meterNums.push_back(meterNum);
            
            for (int fldIndex = 4; fldIndex <= NumAlpha; fldIndex += 2) {
                bool KeyIsStar = (ipsc->cAlphaArgs(fldIndex) == "*" || ipsc->lAlphaFieldBlanks(fldIndex));

                if (ipsc->lAlphaFieldBlanks(fldIndex + 1)) {
                    ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(fldIndex + 1));
                    ShowContinueError(state, "...cannot create custom meter.");
                    BigErrorsFound = true;
                    continue;
                }

                std::string meterOrVarName = UtilityRoutines::MakeUPPERCase(ipsc->cAlphaArgs(fldIndex + 1));
                
                lbrackPos = index(meterOrVarName, '[');
                if (lbrackPos != std::string::npos) meterOrVarName.erase(lbrackPos);

                // This is a meter
                if (auto foundSrcMeter = op->meterMap.find(meterOrVarName); foundSrcMeter != op->meterMap.end()) {
                    int srcMeterNum = foundSrcMeter->second;
                    auto *srcMeter = op->meters[srcMeterNum];
                    assert(srcMeter->type == MeterType::Normal);

                    // Units must match
                    if (meter->units != srcMeter->units) {
                        ShowWarningCustomMessage(state, eoh, format("differing units in {}=\"{}\".",
                                                                    ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName));
                        ShowContinueError(state, format("...will not be shown with the Meter results; units for meter={}, units for this variable={}.",
                                                        Constant::unitNames[(int)meter->units], Constant::unitNames[(int)srcMeter->units]));
                        continue;
                    }
                    
                    if (srcMeterNum == meter->decMeterNum ||
                        std::find(meter->srcMeterNums.begin(), meter->srcMeterNums.end(), srcMeterNum) != meter->srcMeterNums.end()) {
                        ShowWarningCustomMessage(state, eoh, format("{}=\"{}\" is referenced multiple times, only first instance will be used",
                                                                    ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName));
                        continue;
                    }

                    // Link to source meter and its variable and vice versa
                    meter->srcMeterNums.push_back(srcMeterNum);
                    srcMeter->dstMeterNums.push_back(meterNum);
                    meter->srcVarNums.push_back(srcMeter->outVarNum);
                    op->outVars[srcMeter->outVarNum]->meterNums.push_back(meterNum);

                // This is a variable
                } else if (auto foundSrcDDVar = op->ddOutVarMap.find(meterOrVarName); foundSrcDDVar != op->ddOutVarMap.end()) {
                    int srcDDVarNum = foundSrcDDVar->second;
                    auto *srcDDVar = op->ddOutVars[srcDDVarNum];

                    // Has to be a summed variable
                    if (srcDDVar->storeType != StoreType::Summed) {
                        ShowWarningCustomMessage(state, eoh, format("{}=\"{}\" is not summed variable.",
                                                                    ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName));
                        continue;
                    }
                    
                    // It has to match the existing units
                    if (meter->units != srcDDVar->units) {
                        ShowWarningCustomMessage(state, eoh, format("differing units in {}=\"{}\"",
                                                                    ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName));
                        ShowContinueError(state, format("...will not be shown with the Meter results; units for meter={}, units for this variable={}.",
                                                        Constant::unitNames[(int)meter->units], Constant::unitNames[(int)srcDDVar->units]));
                        continue;
                    }
                    
                    if (KeyIsStar) {
                        if (srcDDVar->keyOutVarNums.size() > 0) {
                            for (int keyOutVarNum : srcDDVar->keyOutVarNums) {
                                if (std::find(meter->srcVarNums.begin(), meter->srcVarNums.end(), keyOutVarNum) != meter->srcVarNums.end()) {
                                    ShowWarningCustomMessage(state, eoh, format("Output variable \"{}\" referenced multiple times (directly or via meter)", 
                                                                                op->outVars[keyOutVarNum]->keyColonNameUC));
                                } else {
                                    meter->srcVarNums.push_back(keyOutVarNum);
                                    op->outVars[keyOutVarNum]->meterNums.push_back(meterNum);
                                }
                            }
                        } else {
                            ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName);
                            ErrorsFound = true;
                        }
                    } else { // Key is not "*"
                        bool foundKey = false;
                        for (int keyOutVarNum : srcDDVar->keyOutVarNums) {
                            if (op->outVars[keyOutVarNum]->keyUC == ipsc->cAlphaArgs(fldIndex)) {
                                if (std::find(meter->srcVarNums.begin(), meter->srcVarNums.end(), keyOutVarNum) != meter->srcVarNums.end()) {
                                    ShowWarningCustomMessage(state, eoh, format("Output variable \"{}\" referenced multiple times (directly or via meter)", 
                                                                                op->outVars[keyOutVarNum]->keyColonNameUC));
                                } else {
                                    meter->srcVarNums.push_back(keyOutVarNum);
                                    op->outVars[keyOutVarNum]->meterNums.push_back(meterNum);
                                }
                                foundKey = true;
                                break;
                            }
                        }
                        if (!foundKey) {
                            ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName);
                            ErrorsFound = true;
                        }
                    }

                // This is neither a meter nor a variable
                } else {
                    ShowWarningItemNotFound(state, eoh, ipsc->cAlphaFieldNames(fldIndex + 1), meterOrVarName);
                    ShowContinueError(state, "...will not be shown with the Meter results.");
                    continue;
                }

            } // for (fldIndex)
            
            if (meter->srcVarNums.size() == 0) {
                ShowWarningCustomMessage(state, eoh, format("no items assigned ... will not be shown with the Meter results"));
            }
        }

        if (BigErrorsFound) ErrorsFound = true;
    }

    int AddMeter(EnergyPlusData &state,
                  std::string const &Name,              // Name for the meter
                  Constant::Units const units, // Units for the meter
                  Constant::eResource resource,      // ResourceType for the meter
                  std::string_view const EndUse,            // EndUse for the meter
                  std::string_view const EndUseSub,         // EndUse subcategory for the meter
                  std::string_view const Group, // Group for the meter
                  int const outVarNum 
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine adds a meter to the current definition set of meters.  If the maximum has
        // already been reached, a reallocation procedure begins.  This action needs to be done at the
        // start of the simulation, primarily before any output is stored.

        // Make sure this isn't already in the list of meter names
        auto &op = state.dataOutputProcessor;

        std::string nameUC = UtilityRoutines::MakeUPPERCase(Name);
        
        if (auto found = op->meterMap.find(nameUC); found != op->meterMap.end()) {
            ShowFatalError(state, format("Requested to Add Meter which was already present={}", Name));
            return found->second;
        }
        
        auto *meter = new Meter(Name);
        op->meters.push_back(meter);
        op->meterMap.insert_or_assign(nameUC, op->meters.size() - 1);
        
        meter->resource = resource;
        meter->EndUse = EndUse;
        meter->EndUseSub = EndUseSub;
        meter->Group = Group;
        meter->units = units;
        meter->CurTSValue = 0.0;

        for (int iPeriod = 0; iPeriod < (int)ReportFreq::Num; ++iPeriod) {
            auto &period = meter->periods[iPeriod];
            period.RptNum = ++op->ReportNumberCounter;
        }
        
        for (int iPeriod = 0; iPeriod < (int)ReportFreq::Num; ++iPeriod) {
            auto &period = meter->periods[iPeriod];
            period.accRptNum = ++op->ReportNumberCounter;
        }

        if (meter->resource != Constant::eResource::Invalid) {
            bool errFlag;
            meter->RT_forIPUnits = GetResourceIPUnits(state, meter->resource, units, errFlag);
            if (errFlag) {
                ShowContinueError(state, format("..on Meter=\"{}\".", Name));
                ShowContinueError(state, "..requests for IP units from this meter will be ignored.");
            }
        }

        // Link meter and variable to one another
        meter->outVarNum = outVarNum;
        op->outVars[outVarNum]->meterNums.push_back(op->meters.size() - 1);

        return op->meters.size() - 1;
    }

    int AddCustomMeter(EnergyPlusData &state,
                       std::string const &Name              // Name for the meter
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine adds a meter to the current definition set of meters.

        // Make sure this isn't already in the list of meter names
        auto &op = state.dataOutputProcessor;

        std::string nameUC = UtilityRoutines::MakeUPPERCase(Name);
        
        if (auto found = op->meterMap.find(nameUC); found != op->meterMap.end()) {
            ShowFatalError(state, format("Requested to Add Meter which was already present={}", Name));
            return found->second;
        }
        
        auto *meter = new Meter(Name);
        op->meters.push_back(meter);
        op->meterMap.insert_or_assign(nameUC, op->meters.size() - 1);
        
        for (int iPeriod = 0; iPeriod < (int)ReportFreq::Num; ++iPeriod) {
            auto &period = meter->periods[iPeriod];
            period.RptNum = ++op->ReportNumberCounter;
        }
        
        for (int iPeriod = 0; iPeriod < (int)ReportFreq::Num; ++iPeriod) {
            auto &period = meter->periods[iPeriod];
            period.accRptNum = ++op->ReportNumberCounter;
        }

        return op->meters.size() - 1;
    }
        
    void AttachMeters(EnergyPlusData &state,
                      Constant::Units const units, // Units for this meter
                      Constant::eResource resource,         // Electricity, Gas, etc.
                      std::string_view const EndUse,        // End-use category (Lights, Heating, etc.)
                      std::string_view const EndUseSub,     // End-use subcategory (user-defined, e.g., General Lights, Task Lights, etc.)
                      std::string_view const Group,         // Group key (Facility, Zone, Building, etc.)
                      std::string const &ZoneName,          // Zone key only applicable for Building group
                      std::string const &SpaceType,         // Space Type key only applicable for Building group
                      int const outVarNum                  // Number of this report variable
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines which meters this variable will be on (if any),
        // sets up the meter pointer arrays, and returns a index value to this array which
        // is stored with the variable.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &op = state.dataOutputProcessor;

        std::string_view resourceName = Constant::eResourceNames[(int)resource];

        std::string endUse = standardizeEndUse(EndUse);
        std::string endUseSub = standardizeEndUseSub(endUse, EndUseSub);
        std::string group = standardizeGroup(Group);

        if (!endUseSub.empty())
            addEndUseSubcategory(state, endUse, endUseSub);

        if (!SpaceType.empty())
            addEndUseSpaceType(state, endUse, SpaceType);
        
        std::string meterName = format("{}:Facility", resourceName);
        if (op->meterMap.find(UtilityRoutines::MakeUPPERCase(meterName)) == op->meterMap.end()) {
            AddMeter(state, meterName, units, resource, "", "", "", outVarNum);
        }

        if (!group.empty()) {
            std::string groupMeterName = format("{}:{}", resourceName, Group);
                
            if (op->meterMap.find(groupMeterName) == op->meterMap.end()) {
                AddMeter(state, groupMeterName, units, resource, "", "", Group, outVarNum);
            }

            if (group == "Building") {
                if (!ZoneName.empty()) {
                    std::string zoneMeterName = format("{}:Zone:{}", resourceName, ZoneName);
                    if (op->meterMap.find(zoneMeterName) == op->meterMap.end()) {
                        AddMeter(state, zoneMeterName, units, resource, "", "", "Zone", outVarNum);
                    }
                }
                if (!SpaceType.empty()) {
                    std::string spaceMeterName = format("{}:SpaceType:{}", resourceName, SpaceType);
                    if (op->meterMap.find(spaceMeterName) == op->meterMap.end()) {
                        AddMeter(state, spaceMeterName, units, resource, "", "", "SpaceType", outVarNum);
                    }
                }
            } // if (Group == "Building")
        }

        //!! Following if we do EndUse by ResourceType
        if (!endUse.empty()) {
            std::string enduseMeterName = format("{}:{}", endUse, resourceName);
                
            if (op->meterMap.find(enduseMeterName) == op->meterMap.end()) {
                AddMeter(state, enduseMeterName, units, resource, endUse, "", "", outVarNum);
            }

            if (group == "Building") { // Match to Zone and Space
                if (!ZoneName.empty()) {
                    std::string enduseZoneMeterName = format("{}:{}:Zone:{}", endUse, resourceName, ZoneName);
                    if (op->meterMap.find(UtilityRoutines::MakeUPPERCase(enduseZoneMeterName)) == op->meterMap.end()) {
                        AddMeter(state, enduseZoneMeterName, units, resource, endUse, "", "Zone", outVarNum);
                    }
                }
                if (!SpaceType.empty()) {
                    std::string enduseSpaceMeterName = format("{}:{}:SpaceType:{}", endUse, resourceName, SpaceType);
                    if (op->meterMap.find(UtilityRoutines::MakeUPPERCase(enduseSpaceMeterName)) == op->meterMap.end()) {
                        AddMeter(state, enduseSpaceMeterName, units, resource, endUse, "", "SpaceType", outVarNum);
                    }
                }
            }
        } 

        // End-Use Subcategories
        if (!endUseSub.empty()) {
            std::string subEnduseMeterName = format("{}:{}:{}", endUseSub, endUse, resourceName);
            std::string subEnduseMeterNameUC = UtilityRoutines::MakeUPPERCase(subEnduseMeterName);
            if (op->meterMap.find(subEnduseMeterNameUC) == op->meterMap.end()) {
                AddMeter(state, subEnduseMeterName, units, resource, endUse, endUseSub, "", outVarNum);
            }
            if (group == "Building") { // Match to Zone and Space
                if (!ZoneName.empty()) {
                    std::string subEnduseZoneMeterName = format("{}:{}:{}:Zone:{}", endUseSub, endUse, resourceName, ZoneName);
                    std::string subEnduseZoneMeterNameUC = UtilityRoutines::MakeUPPERCase(subEnduseZoneMeterName);
                    if (op->meterMap.find(subEnduseZoneMeterNameUC) == op->meterMap.end()) {
                        AddMeter(state, subEnduseZoneMeterName, units, resource, endUse, endUseSub, "Zone", outVarNum);
                    }
                }
                if (!SpaceType.empty()) {
                    std::string subEnduseSpaceMeterName = format("{}:{}:{}:SpaceType:{}", endUseSub, endUse, resourceName, SpaceType);
                    std::string subEnduseSpaceMeterNameUC = UtilityRoutines::MakeUPPERCase(subEnduseSpaceMeterName);
                    if (op->meterMap.find(subEnduseSpaceMeterNameUC) == op->meterMap.end()) { 
                        AddMeter(state, subEnduseSpaceMeterName, units, resource, endUse, endUseSub, "SpaceType", outVarNum);
                    }
                }
            }
        } // if (!endUseSub.empty())
    } // AttachMeters()

    //!  Group Meters
    std::string standardizeGroup(std::string_view const groupName)
    {
        std::string group = UtilityRoutines::MakeUPPERCase(groupName);
        if (group == "BUILDING") {
            return "Building";
        } else if (group == "HVAC" || group == "SYSTEM") {
            return "HVAC";
        } else if (group == "PLANT") {
            return "Plant";
        } else {
            return "Junk";
        }
    }

    //!!! EndUse Meter
    std::string standardizeEndUse(std::string_view endUseName)
    {
        assert(!endUseName.empty());
        std::string endUse = UtilityRoutines::MakeUPPERCase(endUseName);

        if (endUse == "INTERIOR LIGHTS" || endUse == "INTERIORLIGHTS") {
            return "InteriorLights";

        } else if (endUse == "EXTERIOR LIGHTS" || endUse == "EXTERIORLIGHTS") {
                return "ExteriorLights";

        } else if (endUse == "HEATING" || endUse == "HTG") {
                return "Heating";
                
        } else if (endUse == "HEATPRODUCED") {
                return "HeatProduced";
                
        } else if (endUse == "COOLING" || endUse == "CLG") {
                return "Cooling";
                
        } else if (endUse == "DOMESTICHOTWATER" || endUse == "DHW" || endUse == "DOMESTIC HOT WATER") {
                return "WaterSystems";
                
        } else if (endUse == "COGEN" || endUse == "COGENERATION") {
                return "Cogeneration";
                
        } else if (endUse == "INTERIOREQUIPMENT" || endUse == "INTERIOR EQUIPMENT") {
                return "InteriorEquipment";
                
        } else if (endUse == "EXTERIOREQUIPMENT" || endUse == "EXTERIOR EQUIPMENT" || endUse == "EXT EQ" ||
                   endUse == "EXTERIOREQ") {
                return "ExteriorEquipment";
                
        } else if (endUse == "EXTERIOR:WATEREQUIPMENT") {
                return "ExteriorEquipment";
                
        } else if (endUse == "PURCHASEDHOTWATER" || endUse == "DISTRICTHOTWATER" || endUse == "PURCHASED HEATING") {
                return "DistrictHotWater";
                
        } else if (endUse == "PURCHASEDCOLDWATER" || endUse == "DISTRICTCHILLEDWATER" || endUse == "PURCHASEDCHILLEDWATER" ||
                   endUse == "PURCHASED COLD WATER" || endUse == "PURCHASED COOLING") {
                return "DistrictChilledWater";
                
        } else if (endUse == "FANS" || endUse == "FAN") {
                return "Fans";

        } else if (endUse == "HEATINGCOILS" || endUse == "HEATINGCOIL" || endUse == "HEATING COILS" ||
                   endUse == "HEATING COIL") {
                return "HeatingCoils";
                
        } else if (endUse == "COOLINGCOILS" || endUse == "COOLINGCOIL" || endUse == "COOLING COILS" ||
                   endUse == "COOLING COIL") {
                return "CoolingCoils";
                
        } else if (endUse == "PUMPS" || endUse == "PUMP") {
                return "Pumps";
                
        } else if (endUse == "FREECOOLING" || endUse == "FREE COOLING") {
                return "Freecooling";
                
        } else if (endUse == "LOOPTOLOOP") {
                return "LoopToLoop";
                
        } else if (endUse == "CHILLERS" || endUse == "CHILLER") {
                return "Chillers";
                
        } else if (endUse == "BOILERS" || endUse == "BOILER") {
                return "Boilers";
                
        } else if (endUse == "BASEBOARD" || endUse == "BASEBOARDS") {
                return "Baseboard";
                
        } else if (endUse == "COOLINGPANEL" || endUse == "COOLINGPANELS") {
                return "CoolingPanel";
                
        } else if (endUse == "HEATREJECTION" || endUse == "HEAT REJECTION") {
                return "HeatRejection";
                
        } else if (endUse == "HUMIDIFIER" || endUse == "HUMIDIFIERS") {
                return "Humidifier";
                
        } else if (endUse == "HEATRECOVERY" || endUse == "HEAT RECOVERY") {
                return "HeatRecovery";
                
        } else if (endUse == "PHOTOVOLTAICS" || endUse == "PV" || endUse == "PHOTOVOLTAIC") {
                return "Photovoltaic";
                
        } else if (endUse == "WINDTURBINES" || endUse == "WT" || endUse == "WINDTURBINE") {
                return "WindTurbine";
                
        } else if (endUse == "ELECTRICSTORAGE") {
                return "ElectricStorage";
                
        } else if (endUse == "POWERCONVERSION") {
                
                return "PowerConversion";
                
        } else if (endUse == "HEAT RECOVERY FOR COOLING" || endUse == "HEATRECOVERYFORCOOLING" ||
                   endUse == "HEATRECOVERYCOOLING") {
                return "HeatRecoveryForCooling";
                
        } else if (endUse == "HEAT RECOVERY FOR HEATING" || endUse == "HEATRECOVERYFORHEATING" ||
                   endUse == "HEATRECOVERYHEATING") {
                return "HeatRecoveryForHeating";
                
        } else if (endUse == "ELECTRICITYEMISSIONS") {
                return "ElectricityEmissions";
                
        } else if (endUse == "PURCHASEDELECTRICITYEMISSIONS") {
                return "PurchasedElectricityEmissions";
                
        } else if (endUse == "SOLDELECTRICITYEMISSIONS") {
                return "SoldElectricityEmissions";
                
        } else if (endUse == "NATURALGASEMISSIONS") {
                return "NaturalGasEmissions";
                
        } else if (endUse == "FUELOILNO1EMISSIONS") {
                return "FuelOilNo1Emissions";
                
        } else if (endUse == "FUELOILNO2EMISSIONS") {
                return "FuelOilNo2Emissions";
                
        } else if (endUse == "COALEMISSIONS") {
                return "CoalEmissions";
                
        } else if (endUse == "GASOLINEEMISSIONS") {
                return "GasolineEmissions";
                
        } else if (endUse == "PROPANEEMISSIONS") {
                return "PropaneEmissions";
                
        } else if (endUse == "DIESELEMISSIONS") {
                return "DieselEmissions";
                
        } else if (endUse == "OTHERFUEL1EMISSIONS") {
                return "OtherFuel1Emissions";
                
        } else if (endUse == "OTHERFUEL2EMISSIONS") {
                return "OtherFuel2Emissions";
                
        } else if (endUse == "CARBONEQUIVALENTEMISSIONS") {
                return "CarbonEquivalentEmissions";
                
        } else if (endUse == "REFRIGERATION") {
                return "Refrigeration";
                
        } else if (endUse == "COLDSTORAGECHARGE") {
                return "ColdStorageCharge";
                
        } else if (endUse == "COLDSTORAGEDISCHARGE") {
                return "ColdStorageDischarge";
                
        } else if (endUse == "WATERSYSTEMS" || endUse == "WATERSYSTEM" || endUse == "Water System") {
                return "WaterSystems";
                
        } else if (endUse == "RAINWATER") {
                return "Rainwater";
                
        } else if (endUse == "CONDENSATE") {
                return "Condensate";
                
        } else if (endUse == "WELLWATER") {
                return "Wellwater";
                
        } else if (endUse == "MAINSWATER" || endUse == "PURCHASEDWATER") {
                return "MainsWater";
                
        } else {
                return "Junk";
        }
    }

    std::string standardizeEndUseSub(std::string_view endUseName, std::string_view endUseSubName)
    {
        if (!endUseSubName.empty()) {
            return std::string(endUseSubName);
        } else if (endUseName.empty()) {
            return "";
        } else if (Constant::EndUse endUse = static_cast<Constant::EndUse>
                   (getEnumerationValue(Constant::endUseNamesUC, UtilityRoutines::MakeUPPERCase(endUseName))); endUse != Constant::EndUse::Invalid) {
            return "General";
        } else {
            return "";
        }
    }
        
    OutputProcessor::RT_IPUnits GetResourceIPUnits(EnergyPlusData &state,
                                                   Constant::eResource resource,      // Resource Type
                                                   Constant::Units const units, // Meter units
                                                   bool &ErrorsFound                  // true if errors found during subroutine
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2012
        //       MODIFIED       September 2012; made into subroutine
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // In order to set up tabular reports for IP units, need to search on same strings
        // that tabular reports does for IP conversion.

        // REFERENCES:
        // OutputReportTabular looks for:
        // CONSUMP - not used in meters
        // ELEC - Electricity (kWH)
        // GAS - Gas (therm)
        // COOL - Cooling (ton)
        // and we need to add WATER (for m3/gal, etc)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        RT_IPUnits IPUnits;

        ErrorsFound = false;

        switch (resource) {
        case Constant::eResource::Electricity:
        case Constant::eResource::ElectricityProduced:
        case Constant::eResource::ElectricityPurchased:
        case Constant::eResource::ElectricitySurplusSold:
        case Constant::eResource::ElectricityNet: {
            IPUnits = RT_IPUnits::Electricity;
        } break;
        case Constant::eResource::NaturalGas: {
            IPUnits = RT_IPUnits::Gas;
        } break;
        case Constant::eResource::Water:
        case Constant::eResource::MainsWater:
        case Constant::eResource::RainWater:
        case Constant::eResource::WellWater:
        case Constant::eResource::OnSiteWater: {
            IPUnits = RT_IPUnits::Water;
        } break;
        case Constant::eResource::DistrictCooling:
        case Constant::eResource::PlantLoopCoolingDemand: {
            IPUnits = RT_IPUnits::Cooling;
        } break;
        default: {
            if (units == Constant::Units::m3) {
                IPUnits = RT_IPUnits::OtherM3;
            } else if (units == Constant::Units::kg) {
                IPUnits = RT_IPUnits::OtherKG;
            } else if (units == Constant::Units::L) {
                IPUnits = RT_IPUnits::OtherL;
            } else {
                IPUnits = RT_IPUnits::OtherJ;
            }
        } break;
        } // switch
        
        //  write(outputfiledebug,*) 'resourcetype=',TRIM(resourcetype)
        //  write(outputfiledebug,*) 'ipunits type=',CodeForIPUnits
        if (units != Constant::Units::kg && units != Constant::Units::J && units != Constant::Units::m3 &&
            units != Constant::Units::L) {
            ShowWarningError(state, format("DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[{}].",
                                           Constant::unitNames[(int)units]));
            ErrorsFound = true;
        }
        return IPUnits;
    }

    void UpdateMeters(EnergyPlusData &state, int const TimeStamp) // Current TimeStamp (for max/min)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2001

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the meters with the current time step value
        // for each meter.  Also, sets min/max values for hourly...run period reporting.

        auto &op = state.dataOutputProcessor;

        if (op->meterValues.size() == 0) {
            return;
        }

        for (int iMeter = 0; iMeter < (int)op->meters.size(); ++iMeter) {
            auto *meter = op->meters[iMeter];
            if (meter->type != MeterType::CustomDec && meter->type != MeterType::CustomDiff) {
                meter->periods[(int)ReportFreq::TimeStep].Value += op->meterValues[iMeter];
            // Is this correct? What is going on here?
            } else {
                meter->periods[(int)ReportFreq::TimeStep].Value =
                        op->meters[meter->decMeterNum]->periods[(int)ReportFreq::TimeStep].Value - op->meterValues[iMeter];
            }

            Real64 TSValue = meter->periods[(int)ReportFreq::TimeStep].Value;
            meter->periods[(int)ReportFreq::Hour].Value += TSValue;
            meter->periods[(int)ReportFreq::Day].Value += TSValue;
            meter->periods[(int)ReportFreq::Month].Value += TSValue;
            meter->periods[(int)ReportFreq::Year].Value += TSValue;
            meter->periods[(int)ReportFreq::Simulation].Value += TSValue;
            if (op->isFinalYear) meter->periodFinYrSM.Value += TSValue;
        }
        
        // Set Max
        for (auto *meter : op->meters) {
            Real64 TSValue = meter->periods[(int)ReportFreq::TimeStep].Value;

            // Todo - HRMinVal, HRMaxVal not used
            auto &periodDY = meter->periods[(int)ReportFreq::Day];
            if (TSValue <= periodDY.MaxVal) continue;
            periodDY.MaxVal = TSValue;
            periodDY.MaxValDate = TimeStamp;

            auto &periodMN = meter->periods[(int)ReportFreq::Month];
            if (TSValue <= periodMN.MaxVal) continue;
            periodMN.MaxVal = TSValue;
            periodMN.MaxValDate = TimeStamp;


            auto &periodYR = meter->periods[(int)ReportFreq::Year];
            if (TSValue > periodYR.MaxVal) {
                periodYR.MaxVal = TSValue;
                periodYR.MaxValDate = TimeStamp;
            }

            auto &periodSM = meter->periods[(int)ReportFreq::Simulation];
            if (TSValue > periodSM.MaxVal) {
                periodSM.MaxVal = TSValue;
                periodSM.MaxValDate = TimeStamp;
            }
            
            if (op->isFinalYear && TSValue > meter->periodFinYrSM.MaxVal) {
                meter->periodFinYrSM.MaxVal = TSValue;
                meter->periodFinYrSM.MaxValDate = TimeStamp;
            }
        } // for (meter)
        
        // Set Min
        for (auto *meter : op->meters) {
            Real64 TSValue = meter->periods[(int)ReportFreq::TimeStep].Value;

            auto &periodDY = meter->periods[(int)ReportFreq::Day];
            if (TSValue >= periodDY.MinVal) continue;
                    
            periodDY.MinVal = TSValue;
            periodDY.MinValDate = TimeStamp;

            auto &periodMN = meter->periods[(int)ReportFreq::Month];
            if (TSValue >= periodMN.MinVal) continue;
            
            periodMN.MinVal = TSValue;
            periodMN.MinValDate = TimeStamp;

            auto &periodYR = meter->periods[(int)ReportFreq::Year];
            if (TSValue < periodYR.MinVal) {
                periodYR.MinVal = TSValue;
                periodYR.MinValDate = TimeStamp;
            }

            auto &periodSM = meter->periods[(int)ReportFreq::Simulation];
            if (TSValue < periodSM.MinVal) {
                periodSM.MinVal = TSValue;
                periodSM.MinValDate = TimeStamp;
            }

            if (op->isFinalYear && TSValue < meter->periodFinYrSM.MinVal) {
                meter->periodFinYrSM.MinVal = TSValue;
                meter->periodFinYrSM.MinValDate = TimeStamp;
            }
        }

        for (int iMeter = 0; iMeter < (int)op->meters.size(); ++iMeter) {
            op->meterValues[iMeter] = 0.0; // Ready for next update
        }
    }

    void ResetAccumulationWhenWarmupComplete(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   June 2015

        // PURPOSE OF THIS SUBROUTINE:
        // Resets the accumulating meter values. Needed after warmup period is over to
        // reset the totals on meters so that they are not accumulated over the warmup period

        auto &op = state.dataOutputProcessor;

        for (auto *meter : op->meters) {
            for (int iPeriod = (int)ReportFreq::Hour; iPeriod < (int)ReportFreq::Num; ++iPeriod) {
                meter->periods[iPeriod].resetVals();
            }
            meter->periodFinYrSM.resetVals();
        }

        for (auto *var : op->outVars) {
            if (var->freq == ReportFreq::Month || var->freq == ReportFreq::Year || var->freq == ReportFreq::Simulation) {
                var->StoreValue = 0.0;
                var->NumStored = 0;
            }
        }
    }

    void ReportTSMeters(EnergyPlusData &state,
                        Real64 const StartMinute, // Start Minute for TimeStep
                        Real64 const EndMinute,   // End Minute for TimeStep
                        bool &PrintESOTimeStamp,  // True if the ESO Time Stamp also needs to be printed
                        bool PrintTimeStampToSQL  // Print Time Stamp to SQL file
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports on the meters that have been requested for
        // reporting on each time step.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool PrintTimeStamp;
        int CurDayType;
        auto &op = state.dataOutputProcessor;
        auto &rf = state.dataResultsFramework->resultsFramework;
        auto &rfMetersTS = rf->Meters[(int)ReportFreq::TimeStep];

        if (!rfMetersTS.dataFrameEnabled()) {
            rf->initializeMeters(op->meters, ReportFreq::TimeStep);
        }

        PrintTimeStamp = true;
        for (int Loop = 0; Loop < (int)op->meters.size(); ++Loop) {
            auto *meter = op->meters[Loop];
            auto &periodTS = meter->periods[(int)ReportFreq::TimeStep];
            meter->CurTSValue = periodTS.Value;
            if (!periodTS.Rpt && !periodTS.accRpt) continue;
            if (PrintTimeStamp) {
                CurDayType = state.dataEnvrn->DayOfWeek;
                if (state.dataEnvrn->HolidayIndex > 0) {
                    CurDayType = state.dataEnvrn->HolidayIndex;
                }
                WriteTimeStampFormatData(state,
                                         state.files.mtr,
                                         ReportFreq::EachCall,
                                         op->freqStampReportNums[(int)ReportFreq::TimeStep],
                                         state.dataGlobal->DayOfSimChr,
                                         PrintTimeStamp && PrintTimeStampToSQL,
                                         state.dataEnvrn->Month,
                                         state.dataEnvrn->DayOfMonth,
                                         state.dataGlobal->HourOfDay,
                                         EndMinute,
                                         StartMinute,
                                         state.dataEnvrn->DSTIndicator,
                                         ScheduleManager::dayTypeNames[CurDayType]);
                if (rfMetersTS.dataFrameEnabled()) {
                    rfMetersTS.newRow(
                        state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, EndMinute, state.dataGlobal->CalendarYear);
                }
                PrintTimeStamp = false;
                PrintTimeStampToSQL = false;
            }

            if (PrintESOTimeStamp && !periodTS.RptFO && !periodTS.accRptFO) {
                CurDayType = (state.dataEnvrn->HolidayIndex > 0) ? state.dataEnvrn->HolidayIndex : state.dataEnvrn->DayOfWeek;
                WriteTimeStampFormatData(state,
                                         state.files.eso,
                                         ReportFreq::EachCall,
                                         op->freqStampReportNums[(int)ReportFreq::TimeStep],
                                         state.dataGlobal->DayOfSimChr,
                                         PrintTimeStamp && PrintESOTimeStamp && PrintTimeStampToSQL,
                                         state.dataEnvrn->Month,
                                         state.dataEnvrn->DayOfMonth,
                                         state.dataGlobal->HourOfDay,
                                         EndMinute,
                                         StartMinute,
                                         state.dataEnvrn->DSTIndicator,
                                         ScheduleManager::dayTypeNames[CurDayType]);
                PrintESOTimeStamp = false;
            }

            if (periodTS.Rpt) {
                WriteReportMeterData(state,
                                     periodTS.RptNum,
                                     periodTS.Value,
                                     ReportFreq::TimeStep,
                                     0.0,
                                     0,
                                     0.0,
                                     0,
                                     periodTS.RptFO);
                rfMetersTS.pushVariableValue(periodTS.RptNum, periodTS.Value);
            }

            if (periodTS.accRpt) {
                WriteCumulativeReportMeterData(state, periodTS.accRptNum, periodTS.Value, periodTS.accRptFO);
                rfMetersTS.pushVariableValue(periodTS.accRptNum, periodTS.Value);
            }
        }

        for (auto *meter : op->meters) {
            meter->periods[(int)ReportFreq::TimeStep].Value = 0.0;
        }
    }

    void ReportMeters(EnergyPlusData &state,
                      ReportFreq freq,
                      bool PrintTimeStampToSQL // Print Time Stamp to SQL file
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports on the meters that have been requested for
        // reporting on each hour.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool PrintTimeStamp;
        int CurDayType;
        auto &op = state.dataOutputProcessor;
        auto &rf = state.dataResultsFramework->resultsFramework;
        auto &rfMeters = rf->Meters[(int)freq];

        assert(freq == ReportFreq::Hour || freq == ReportFreq::Day || freq == ReportFreq::Month ||
               freq == ReportFreq::Year || freq == ReportFreq::Simulation);
        
        if (!rfMeters.dataFrameEnabled()) {
            rf->initializeMeters(op->meters, freq);
        }

        PrintTimeStamp = true;
        for (auto *meter : op->meters) {
            auto &period = meter->periods[(int)freq];

            if (freq == ReportFreq::Simulation) {
                meter->periodLastSM.Value = period.Value;
                meter->periodLastSM.MinVal = period.MinVal;
                meter->periodLastSM.MinValDate = period.MinValDate;
                meter->periodLastSM.MaxVal = period.MaxVal;
                meter->periodLastSM.MaxValDate = period.MaxValDate;
            }
            
            if (!period.Rpt && !period.accRpt) continue;
            if (PrintTimeStamp) {
                CurDayType = (state.dataEnvrn->HolidayIndex > 0) ? state.dataEnvrn->HolidayIndex : state.dataEnvrn->DayOfWeek;

                switch (freq) {

                case ReportFreq::Hour: {
                     WriteTimeStampFormatData(state,
                                              state.files.mtr,
                                              freq, 
                                              op->freqStampReportNums[(int)freq],
                                              state.dataGlobal->DayOfSimChr,
                                              PrintTimeStamp && PrintTimeStampToSQL,
                                              state.dataEnvrn->Month,
                                              state.dataEnvrn->DayOfMonth,
                                              state.dataGlobal->HourOfDay,
                                              _,
                                              _,
                                              state.dataEnvrn->DSTIndicator,
                                              ScheduleManager::dayTypeNames[CurDayType]);
                } break;

                case ReportFreq::Day: {
                    WriteTimeStampFormatData(state,
                                             state.files.mtr,
                                             freq,
                                             op->freqStampReportNums[(int)freq],
                                             state.dataGlobal->DayOfSimChr,
                                             PrintTimeStamp && PrintTimeStampToSQL,
                                             state.dataEnvrn->Month,
                                             state.dataEnvrn->DayOfMonth,
                                             _,
                                             _,
                                             _,
                                             state.dataEnvrn->DSTIndicator,
                                             ScheduleManager::dayTypeNames[CurDayType]);
                } break;

                case ReportFreq::Month: {
                    WriteTimeStampFormatData(state,
                                             state.files.mtr,
                                             freq,
                                             op->freqStampReportNums[(int)freq],
                                             state.dataGlobal->DayOfSimChr,
                                             PrintTimeStamp && PrintTimeStampToSQL,
                                             state.dataEnvrn->Month);
                } break;

                case ReportFreq::Year: {
                    WriteYearlyTimeStamp(state,
                                         state.files.mtr,
                                         op->freqStampReportNums[(int)freq],
                                         state.dataGlobal->CalendarYearChr,
                                         PrintTimeStamp && PrintTimeStampToSQL);
                } break;
                        
                case ReportFreq::Simulation: {
                    WriteTimeStampFormatData(state,
                                             state.files.mtr,
                                             freq,
                                             op->freqStampReportNums[(int)freq],
                                             state.dataGlobal->DayOfSimChr,
                                             PrintTimeStamp && PrintTimeStampToSQL);
                } break;

                default: {
                } break;
                } // switch (freq)

                if (rfMeters.dataFrameEnabled()) {
                    rfMeters.newRow(state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0, state.dataGlobal->CalendarYear);
                }
                PrintTimeStamp = false;
                PrintTimeStampToSQL = false;
            }

            if (period.Rpt) {
                WriteReportMeterData(state,
                                     period.RptNum,
                                     period.Value,
                                     freq,
                                     (freq == ReportFreq::Hour) ? 0.0 : period.MinVal,
                                     (freq == ReportFreq::Hour) ? 0.0 : period.MinValDate,
                                     (freq == ReportFreq::Hour) ? 0.0 : period.MaxVal,
                                     (freq == ReportFreq::Hour) ? 0.0 : period.MaxValDate,
                                     period.RptFO); // EnergyMeters(Loop)%HRMinVal, EnergyMeters(Loop)%HRMinValDate, & |
                                                                      // EnergyMeters(Loop)%HRMaxVal, EnergyMeters(Loop)%HRMaxValDate, &
                rfMeters.pushVariableValue(period.RptNum, period.Value);
                period.Value = 0.0;
                period.MinVal = MinSetValue;
                period.MaxVal = MaxSetValue;
            }

            if (period.accRpt) {
                WriteCumulativeReportMeterData(state, period.accRptNum, meter->periods[(int)ReportFreq::Simulation].Value, period.accRptFO);
                rfMeters.pushVariableValue(period.accRptNum, meter->periods[(int)ReportFreq::Simulation].Value);
            }
        } // for (meter)
    } // ReportMeters()

    void ReportForTabularReports(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   August 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is called after all the simulation is done and before
        // tabular reports in order to reduce the number of calls to the predefined routine
        // for SM (Simulation period) meters, the value of the last calculation is stored
        // in the data structure.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &op = state.dataOutputProcessor;

        for (auto *meter : op->meters) {
            auto &period = meter->periodFinYrSM;
                
            switch (meter->RT_forIPUnits) {
            case RT_IPUnits::Electricity: {
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMelecannual, meter->Name, period.Value * Constant::convertJtoGJ);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMelecminvalue, meter->Name, period.MinVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMelecminvaluetime, meter->Name, DateToStringWithMonth(period.MinValDate));
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMelecmaxvalue, meter->Name, period.MaxVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMelecmaxvaluetime, meter->Name, DateToStringWithMonth(period.MaxValDate));
            } break;

            case RT_IPUnits::Gas: {
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMgasannual, meter->Name, period.Value * Constant::convertJtoGJ);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMgasminvalue, meter->Name, period.MinVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMgasminvaluetime, meter->Name, DateToStringWithMonth(period.MinValDate));
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMgasmaxvalue, meter->Name, period.MaxVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMgasmaxvaluetime, meter->Name, DateToStringWithMonth(period.MaxValDate));
            } break;

            case RT_IPUnits::Cooling: {
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMcoolannual, meter->Name, period.Value * Constant::convertJtoGJ);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMcoolminvalue, meter->Name, period.MinVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMcoolminvaluetime, meter->Name, DateToStringWithMonth(period.MinValDate));
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMcoolmaxvalue, meter->Name, period.MaxVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMcoolmaxvaluetime, meter->Name, DateToStringWithMonth(period.MaxValDate));
            } break;
                    
            case RT_IPUnits::Water: {
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMwaterannual, meter->Name, period.Value);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMwaterminvalue, meter->Name, period.MinVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMwaterminvaluetime, meter->Name, DateToStringWithMonth(period.MinValDate));
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMwatermaxvalue, meter->Name, period.MaxVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMwatermaxvaluetime, meter->Name, DateToStringWithMonth(period.MaxValDate));
            } break;
                    
            case RT_IPUnits::OtherKG: {
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherKGannual, meter->Name, period.Value);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherKGminvalue, meter->Name, period.MinVal / state.dataGlobal->TimeStepZoneSec, 3);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherKGminvaluetime, meter->Name, DateToStringWithMonth(period.MinValDate));
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherKGmaxvalue, meter->Name, period.MaxVal / state.dataGlobal->TimeStepZoneSec, 3);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherKGmaxvaluetime, meter->Name, DateToStringWithMonth(period.MaxValDate));
            } break;
                    
            case RT_IPUnits::OtherM3: {
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherM3annual, meter->Name, period.Value, 3);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherM3minvalue, meter->Name, period.MinVal / state.dataGlobal->TimeStepZoneSec, 3);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherM3minvaluetime, meter->Name, DateToStringWithMonth(period.MinValDate));
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherM3maxvalue, meter->Name, period.MaxVal / state.dataGlobal->TimeStepZoneSec, 3);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherM3maxvaluetime, meter->Name, DateToStringWithMonth(period.MaxValDate));
            } break;
                    
            case RT_IPUnits::OtherL: {
                OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherLannual, meter->Name, period.Value, 3);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherLminvalue, meter->Name, period.MinVal / state.dataGlobal->TimeStepZoneSec, 3);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherLminvaluetime, meter->Name, DateToStringWithMonth(period.MinValDate));
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherLmaxvalue, meter->Name, period.MaxVal / state.dataGlobal->TimeStepZoneSec, 3);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherLmaxvaluetime, meter->Name, DateToStringWithMonth(period.MaxValDate));
            } break;

            default: {
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherJannual, meter->Name, period.Value * Constant::convertJtoGJ);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherJminvalue, meter->Name, period.MinVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherJminvaluetime, meter->Name, DateToStringWithMonth(period.MinValDate));
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherJmaxvalue, meter->Name, period.MaxVal / state.dataGlobal->TimeStepZoneSec);
                OutputReportPredefined::PreDefTableEntry(
                    state, state.dataOutRptPredefined->pdchEMotherJmaxvaluetime, meter->Name, DateToStringWithMonth(period.MaxValDate));
            } break;
            } // switch
        } // for (meter)
    } // ReportForTabularReports()

    std::string DateToStringWithMonth(int const codedDate) // word containing encoded month, day, hour, minute
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   August 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //   Convert the coded date format into a usable
        //   string

        if (codedDate == 0) return "-";

        static constexpr std::string_view DateFmt("{:02}-{:3}-{:02}:{:02}");

        // ((month*100 + day)*100 + hour)*100 + minute
        int Month;  // month in integer format (1-12)
        int Day;    // day in integer format (1-31)
        int Hour;   // hour in integer format (1-24)
        int Minute; // minute in integer format (0:59)

        General::DecodeMonDayHrMin(codedDate, Month, Day, Hour, Minute);

        if (Month < 1 || Month > 12) return "-";
        if (Day < 1 || Day > 31) return "-";
        if (Hour < 1 || Hour > 24) return "-";
        if (Minute < 0 || Minute > 60) return "-";

        --Hour;
        if (Minute == 60) {
            ++Hour;
            Minute = 0;
        }

        std::string monthName;
        switch (Month) {
        case 1:
            monthName = "JAN";
            break;
        case 2:
            monthName = "FEB";
            break;
        case 3:
            monthName = "MAR";
            break;
        case 4:
            monthName = "APR";
            break;
        case 5:
            monthName = "MAY";
            break;
        case 6:
            monthName = "JUN";
            break;
        case 7:
            monthName = "JUL";
            break;
        case 8:
            monthName = "AUG";
            break;
        case 9:
            monthName = "SEP";
            break;
        case 10:
            monthName = "OCT";
            break;
        case 11:
            monthName = "NOV";
            break;
        case 12:
            monthName = "DEC";
            break;
        default:
            assert(false);
        }

        return format(DateFmt, Day, monthName, Hour, Minute);
    }


    std::string OutVar::multiplierString() const
    {
        return (ZoneMult == 1 && ZoneListMult == 1) ? "" :
                format(" * {} (Zone Multiplier = {}, Zone List Multiplier = {})", ZoneMult * ZoneListMult, ZoneMult, ZoneListMult);
    }
        
    void ReportMeterDetails(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2006

        // PURPOSE OF THIS SUBROUTINE:
        // Writes the meter details report.  This shows which variables are on
        // meters as well as the meter contents.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &op = state.dataOutputProcessor;

        for (auto const *var : op->outVars) {    

            if (var->meterNums.size() == 0) continue;

            print(state.files.mtd,
                  "\n Meters for {},{}{}{}\n",
                  var->ReportID,
                  var->keyColonName,
                  Constant::unitNames[(int)var->units],
                  var->multiplierString());

            for (int const meterNum : var->meterNums) {
                print(state.files.mtd, "  On{}Meter={}\n", (op->meters[meterNum]->type == MeterType::Normal) ? "" : "Custom", op->meters[meterNum]->Name);
            }
        } // for (var)


        for (auto const *meter : op->meters) {
                
            print(state.files.mtd, "\n For Meter={} [{}]", meter->Name, Constant::unitNames[(int)meter->units]);
            if (meter->resource == Constant::eResource::Invalid) {
                print(state.files.mtd, ", ResourceType={}", Constant::eResourceNames[(int)meter->resource]);
            }
            if (!meter->EndUse.empty()) {
                print(state.files.mtd, ", EndUse={}", meter->EndUse);
            }
            if (!meter->Group.empty()) {
                print(state.files.mtd, ", Group={}", meter->Group);
            }
            print(state.files.mtd, ", contents are:\n");

            if (meter->type == MeterType::Normal) {
                auto const *var = op->outVars[meter->outVarNum];
                print(state.files.mtd, "  {}{}\n", var->keyColonName, var->multiplierString());

            } else if (meter->type == MeterType::Custom) {
                for (int srcVarNum : meter->srcVarNums) {
                    auto const *var = op->outVars[srcVarNum];
                    print(state.files.mtd, "  {}{}\n", var->keyColonName, var->multiplierString());
                }

            } else if (meter->type == MeterType::CustomDec) {
                print(state.files.mtd, " Values for this meter will be Source Meter=\"{}\" decremented by:\n", op->meters[meter->decMeterNum]->Name);
                for (int srcVarNum : meter->srcVarNums) {
                    auto const *var = op->outVars[srcVarNum];
                    print(state.files.mtd, "  {}{}\n", var->keyColonName, var->multiplierString());
                }
            } 
        } // for (meter)
    } // ReportMeterDetails()

    // *****************************************************************************
    // End of routines for Energy Meters implementation in EnergyPlus.
    // *****************************************************************************

    void WriteTimeStampFormatData(
        EnergyPlusData &state,
        InputOutputFile &outputFile,
        ReportFreq const reportingInterval, // See Module Parameter Definitions for ReportEach, ReportTimeStep, ReportHourly, etc.
        int const reportID,                         // The ID of the time stamp
        std::string const &DayOfSimChr,             // the number of days simulated so far
        bool writeToSQL,
        ObjexxFCL::Optional_int_const Month,           // the month of the reporting interval
        ObjexxFCL::Optional_int_const DayOfMonth,      // The day of the reporting interval
        ObjexxFCL::Optional_int_const Hour,            // The hour of the reporting interval
        ObjexxFCL::Optional<Real64 const> EndMinute,   // The last minute in the reporting interval
        ObjexxFCL::Optional<Real64 const> StartMinute, // The starting minute of the reporting interval
        ObjexxFCL::Optional_int_const DST,             // A flag indicating whether daylight savings time is observed
        ObjexxFCL::Optional_string_const DayType       // The day tied for the data (e.g., Monday)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   July 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function reports the timestamp data for the output processor
        // Much of the code in this function was embedded in earlier versions of EnergyPlus
        // and was moved to this location to simplify maintenance and to allow for data output
        // to the SQL database

        assert(std::to_string(reportID).length() + DayOfSimChr.length() + (DayType.present() ? DayType().length() : 0u) + 26 <
               N_WriteTimeStampFormatData); // Check will fit in stamp size

        if (!outputFile.good()) return;

        switch (reportingInterval) {
        case ReportFreq::EachCall:
        case ReportFreq::TimeStep:
            print<FormatSyntax::FMT>(outputFile,
                                     "{},{},{:2d},{:2d},{:2d},{:2d},{:5.2f},{:5.2f},{}\n",
                                     reportID,
                                     DayOfSimChr.c_str(),
                                     Month(),
                                     DayOfMonth(),
                                     DST(),
                                     Hour(),
                                     StartMinute(),
                                     EndMinute(),
                                     DayType().c_str());
            if (writeToSQL && state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord((int)reportingInterval,
                                                                                reportID,
                                                                                state.dataGlobal->DayOfSim,
                                                                                state.dataEnvrn->CurEnvirNum,
                                                                                state.dataGlobal->CalendarYear,
                                                                                Month,
                                                                                DayOfMonth,
                                                                                Hour,
                                                                                EndMinute,
                                                                                StartMinute,
                                                                                DST,
                                                                                DayType,
                                                                                state.dataGlobal->WarmupFlag);
            }
            break;
        case ReportFreq::Hour:
            print<FormatSyntax::FMT>(outputFile,
                                     "{},{},{:2d},{:2d},{:2d},{:2d},{:5.2f},{:5.2f},{}\n",
                                     reportID,
                                     DayOfSimChr.c_str(),
                                     Month(),
                                     DayOfMonth(),
                                     DST(),
                                     Hour(),
                                     0.0,
                                     60.0,
                                     DayType().c_str());
            if (writeToSQL && state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord((int)reportingInterval,
                                                                                reportID,
                                                                                state.dataGlobal->DayOfSim,
                                                                                state.dataEnvrn->CurEnvirNum,
                                                                                state.dataGlobal->CalendarYear,
                                                                                Month,
                                                                                DayOfMonth,
                                                                                Hour,
                                                                                _,
                                                                                _,
                                                                                DST,
                                                                                DayType,
                                                                                state.dataGlobal->WarmupFlag);
            }
            break;
        case ReportFreq::Day:
            print<FormatSyntax::FMT>(outputFile,
                                     "{},{},{:2d},{:2d},{:2d},{}\n",
                                     reportID,
                                     DayOfSimChr.c_str(),
                                     Month(),
                                     DayOfMonth(),
                                     DST(),
                                     DayType().c_str());
            if (writeToSQL && state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord((int)reportingInterval,
                                                                                reportID,
                                                                                state.dataGlobal->DayOfSim,
                                                                                state.dataEnvrn->CurEnvirNum,
                                                                                state.dataGlobal->CalendarYear,
                                                                                Month,
                                                                                DayOfMonth,
                                                                                _,
                                                                                _,
                                                                                _,
                                                                                DST,
                                                                                DayType,
                                                                                state.dataGlobal->WarmupFlag);
            }
            break;
        case ReportFreq::Month:
            print<FormatSyntax::FMT>(outputFile, "{},{},{:2d}\n", reportID, DayOfSimChr.c_str(), Month());
            if (writeToSQL && state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord((int)reportingInterval,
                                                                                reportID,
                                                                                state.dataGlobal->DayOfSim,
                                                                                state.dataEnvrn->CurEnvirNum,
                                                                                state.dataGlobal->CalendarYear,
                                                                                Month);
            }
            break;
        case ReportFreq::Simulation:
            print<FormatSyntax::FMT>(outputFile, "{},{}\n", reportID, DayOfSimChr.c_str());
            if (writeToSQL && state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->createSQLiteTimeIndexRecord((int)reportingInterval,
                                                                                reportID,
                                                                                state.dataGlobal->DayOfSim,
                                                                                state.dataEnvrn->CurEnvirNum,
                                                                                state.dataGlobal->CalendarYear);
            }
            break;
        default:
            if (state.dataSQLiteProcedures->sqlite) {
                state.dataSQLiteProcedures->sqlite->sqliteWriteMessage(format<FormatSyntax::FMT>(
                    "Illegal reportingInterval passed to WriteTimeStampFormatData: {}", (int)reportingInterval));
            }
            break;
        }
    }

    void WriteYearlyTimeStamp(EnergyPlusData &state,
                              InputOutputFile &outputFile,
                              int reportID, // The ID of the time stamp
                              std::string const &yearOfSimChr,   // the year of the simulation
                              bool writeToSQL)
    {
        print(outputFile, "{},{}\n", reportID, yearOfSimChr);
        if (writeToSQL && state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createYearlyTimeIndexRecord(state.dataGlobal->CalendarYear, state.dataEnvrn->CurEnvirNum);
        }
    }

    void WriteReportVariableDictionaryItem(EnergyPlusData &state,
                                           ReportFreq const freq, // The reporting interval (e.g., hourly, daily)
                                           StoreType const storeType,
                                           int const reportID,                       // The reporting ID for the data
                                           [[maybe_unused]] int const indexGroupKey, // The reporting group (e.g., Zone, Plant Loop, etc.)
                                           std::string const &indexGroup,            // The reporting group (e.g., Zone, Plant Loop, etc.)
                                           std::string_view const keyedValue,        // The key name for the data
                                           std::string_view const variableName,      // The variable's actual name
                                           TimeStepType const timeStepType,
                                           Constant::Units const units, // The variables units
                                           std::string_view const customUnitName,
                                           std::string_view const ScheduleName)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   August 2008
        //       MODIFIED       April 2011; Linda Lawrie
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes the ESO data dictionary information to the output files
        // and the SQL database

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &op = state.dataOutputProcessor;
        auto &rf = state.dataResultsFramework->resultsFramework;
        
        std::string_view unitsString = (units == Constant::Units::customEMS && !customUnitName.empty()) ? customUnitName : Constant::unitNames[(int)units];

        if (state.files.eso.good()) {
            print(state.files.eso, "{},{},{},{} [{}]{}{}{}\n",
                  reportID, reportFreqArbitraryInts[(int)freq], keyedValue, variableName, unitsString,
                  reportingFrequencyNoticeStrings[(int)freq], !ScheduleName.empty() ? "," : "", ScheduleName);
        }

        if (freq == ReportFreq::Hour || freq == ReportFreq::Day || freq == ReportFreq::Month || freq == ReportFreq::Year || freq == ReportFreq::Simulation)
            op->freqTrackingVariables[(int)freq] = true;                

        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(reportID,
                                                                                   (int)storeType,
                                                                                   indexGroup,
                                                                                   keyedValue,
                                                                                   variableName,
                                                                                   (int)timeStepType,
                                                                                   unitsString,
                                                                                   (int)freq,
                                                                                   false,
                                                                                   ScheduleName);
        }

        rf->addReportVariable(keyedValue, variableName, unitsString, freq);

        // add to ResultsFramework for output variable list, need to check RVI/MVI later
    }

    void WriteMeterDictionaryItem(EnergyPlusData &state,
                                  ReportFreq const freq, // The reporting interval (e.g., hourly, daily)
                                  StoreType const storeType,
                                  int const reportID,                       // The reporting ID in for the variable
                                  [[maybe_unused]] int const indexGroupKey, // The reporting group for the variable
                                  std::string const &indexGroup,            // The reporting group for the variable
                                  std::string const &meterName,             // The variable's meter name
                                  Constant::Units const units,              // The variables units
                                  bool const cumulativeMeterFlag,           // A flag indicating cumulative data
                                  bool const meterFileOnlyFlag              // A flag indicating whether the data is to be written to standard output
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   August 2008
        //       MODIFIED       April 2011; Linda Lawrie
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // The subroutine writes meter data dictionary information to the output files
        // and the SQL database. Much of the code here was embedded in other subroutines
        // and was moved here for the purposes of ease of maintenance and to allow easy
        // data reporting to the SQL database

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &rf = state.dataResultsFramework->resultsFramework;

        std::string FreqString = std::string(reportingFrequencyNoticeStrings[(int)freq]);
        std::string FreqString2 = FreqString.substr(0, index(FreqString, '['));

        const auto print_meter = [&](EnergyPlusData &state, const int frequency) {
            const auto out = [&](InputOutputFile &of) {
                if (of.good()) {
                    if (cumulativeMeterFlag) {
                        static constexpr std::string_view fmt = "{},{},Cumulative {} [{}]{}\n";
                        print(of, fmt, reportID, 1, meterName, Constant::unitNames[(int)units], FreqString2);
                    } else {
                        static constexpr std::string_view fmt = "{},{},{} [{}]{}\n";
                        print(of, fmt, reportID, frequency, meterName, Constant::unitNames[(int)units], FreqString);
                    }
                }
            };

            out(state.files.mtr);
            if (!meterFileOnlyFlag) {
                out(state.files.eso);
            }
        };

        print_meter(state, reportFreqArbitraryInts[(int)freq]); 

        static constexpr std::string_view keyedValueStringCum("Cumulative ");
        static constexpr std::string_view keyedValueStringNon;
        std::string_view const keyedValueString(cumulativeMeterFlag ? keyedValueStringCum : keyedValueStringNon);

        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteReportDictionaryRecord(reportID,
                                                                                   (int)storeType,
                                                                                   indexGroup,
                                                                                   keyedValueString,
                                                                                   meterName,
                                                                                   1,
                                                                                   Constant::unitNames[(int)units],
                                                                                   (int)freq,
                                                                                   true);
        }

        rf->addReportMeter(meterName, Constant::unitNames[(int)units], freq);
        // add to ResultsFramework for output variable list, need to check RVI/MVI later
    }

    void WriteRealVariableOutput(EnergyPlusData &state,
                                 OutVarReal *realVar,             // Real variable to write out
                                 ReportFreq const reportFreq // The report type or interval (e.g., hourly)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   August 2008
        //       MODIFIED       April 2011; Linda Lawrie, December 2017; Jason DeGraw
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes real report variable data to the output file and
        // SQL database. Much of the code here was an included in earlier versions
        // of the UpdateDataandReport subroutine. The code was moved to facilitate
        // easier maintenance and writing of data to the SQL database.

        if (realVar->Report && realVar->freq == reportFreq && realVar->Stored) {
            if (realVar->NumStored > 0.0) {
                WriteReportRealData(state,
                                    realVar->ReportID,
                                    realVar->StoreValue,
                                    realVar->storeType,
                                    realVar->NumStored,
                                    realVar->freq,
                                    realVar->MinValue,
                                    realVar->minValueDate,
                                    realVar->MaxValue,
                                    realVar->maxValueDate);
                ++state.dataGlobal->StdOutputRecordCount;
            }

            realVar->StoreValue = 0.0;
            realVar->NumStored = 0.0;
            realVar->MinValue = MinSetValue;
            realVar->MaxValue = MaxSetValue;
            realVar->Stored = false;
        }
    }

    void WriteReportRealData(EnergyPlusData &state,
                             int const reportID,
                             Real64 const repValue,
                             StoreType const storeType,
                             Real64 const numOfItemsStored,
                             ReportFreq const freq,
                             Real64 const minValue,
                             int const minValueDate,
                             Real64 const MaxValue,
                             int const maxValueDate)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   July 2008
        //       MODIFIED       April 2011; Linda Lawrie
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes the average real data to the output files and
        // SQL database. It supports the WriteRealVariableOutput subroutine.
        // Much of the code here was an included in earlier versions
        // of the UpdateDataandReport subroutine. The code was moved to facilitate
        // easier maintenance and writing of data to the SQL database.

        std::string NumberOut;   // Character for producing "number out"
        Real64 repVal(repValue); // The variable's value
        auto &rf = state.dataResultsFramework->resultsFramework;
        
        if (storeType == StoreType::Averaged) {
            repVal /= numOfItemsStored;
        }
        if (repVal == 0.0) {
            NumberOut = "0.0";
        } else {
            dtoa(repVal, state.dataOutputProcessor->s_WriteReportRealData);
            NumberOut = std::string(state.dataOutputProcessor->s_WriteReportRealData);
        }

        if (rf->timeSeriesEnabled()) {
            //// The others (<= hourly) are handled inline with the code
            if (freq == ReportFreq::Day || freq == ReportFreq::Month || freq == ReportFreq::Year || freq == ReportFreq::Simulation) {
                rf->freqTSData[(int)freq].pushVariableValue(reportID, repVal);
            }
        }

        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteReportDataRecord(
                reportID, repVal, (int)freq, minValue, minValueDate, MaxValue, maxValueDate);
        }

        if ((freq == ReportFreq::EachCall) || (freq == ReportFreq::TimeStep) || (freq == ReportFreq::Hour)) { // -1, 0, 1
            if (state.files.eso.good()) {
                print(state.files.eso, "{},{}\n", reportID, NumberOut);
            }

        } else { // if ( ( reportingInterval == ReportFreq::Daily ) || ( reportingInterval == ReportFreq::Monthly ) || (
                 // reportingInterval == ReportFreq::Simulation ) ) { //  2, 3, 4, 5
            std::string MaxOut; // Character for Max out string
            std::string MinOut; // Character for Min out string

            if (MaxValue == 0.0) {
                MaxOut = "0.0";
            } else {
                dtoa(MaxValue, state.dataOutputProcessor->s_WriteReportRealData);
                MaxOut = std::string(state.dataOutputProcessor->s_WriteReportRealData);
            }

            if (minValue == 0.0) {
                MinOut = "0.0";
            } else {
                dtoa(minValue, state.dataOutputProcessor->s_WriteReportRealData);
                MinOut = std::string(state.dataOutputProcessor->s_WriteReportRealData);
            }

            // Append the min and max strings with date information
            ProduceMinMaxString(MinOut, minValueDate, freq);
            ProduceMinMaxString(MaxOut, maxValueDate, freq);

            if (state.files.eso.good()) {
                print(state.files.eso, "{},{},{},{}\n", reportID, NumberOut, MinOut, MaxOut);
            }
        }
    }

    void WriteCumulativeReportMeterData(EnergyPlusData &state,
                                        int const reportID,           // The variable's report ID
                                        Real64 const repValue,        // The variable's value
                                        bool const meterOnlyFlag      // A flag that indicates if the data should be written to standard output
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   July 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes the cumulative meter data to the output files and
        // SQL database.

        std::string NumberOut; // Character for producing "number out"

        if (repValue == 0.0) {
            NumberOut = "0.0";
        } else {
            dtoa(repValue, state.dataOutputProcessor->s_WriteCumulativeReportMeterData);
            NumberOut = std::string(state.dataOutputProcessor->s_WriteCumulativeReportMeterData);
        }

        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteReportDataRecord(reportID, repValue);
        }

        if (state.files.mtr.good()) print(state.files.mtr, "{},{}\n", reportID, NumberOut);
        ++state.dataGlobal->StdMeterRecordCount;

        if (!meterOnlyFlag) {
            if (state.files.eso.good()) print(state.files.eso, "{},{}\n", reportID, NumberOut);
            ++state.dataGlobal->StdOutputRecordCount;
        }
    }

    void WriteReportMeterData(EnergyPlusData &state,
                              int const reportID,                         // The variable's report ID
                              Real64 const repValue,                      // The variable's value
                              ReportFreq const freq,                      // The variable's reporting interval (e.g., hourly)
                              Real64 const minValue,                      // The variable's minimum value during the reporting interval
                              int const minValueDate,                     // The date the minimum value occurred
                              Real64 const MaxValue,                      // The variable's maximum value during the reporting interval
                              int const maxValueDate,                     // The date of the maximum value
                              bool const meterOnlyFlag                    // Indicates whether the data is for the meter file only
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   July 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes for the non-cumulative meter data to the output files and
        // SQL database.

        std::string NumberOut; // Character for producing "number out"

        if (repValue == 0.0) {
            NumberOut = "0.0";
        } else {
            dtoa(repValue, state.dataOutputProcessor->s_WriteReportMeterData);
            NumberOut = std::string(state.dataOutputProcessor->s_WriteReportMeterData);
        }

        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteReportDataRecord(reportID,
                                                                             repValue,
                                                                             (int)freq,
                                                                             minValue,
                                                                             minValueDate,
                                                                             MaxValue,
                                                                             maxValueDate,
                                                                             state.dataGlobal->MinutesPerTimeStep);
        }

        if ((freq == ReportFreq::EachCall) || (freq == ReportFreq::TimeStep) || (freq == ReportFreq::Hour)) { // -1, 0, 1
            if (state.files.mtr.good()) {
                print(state.files.mtr, "{},{}\n", reportID, NumberOut);
            }
            ++state.dataGlobal->StdMeterRecordCount;
            if (state.files.eso.good() && !meterOnlyFlag) {
                print(state.files.eso, "{},{}\n", reportID, NumberOut);
                ++state.dataGlobal->StdOutputRecordCount;
            }
        } else { // if ( ( reportingInterval == ReportDaily ) || ( reportingInterval == ReportMonthly ) || ( reportingInterval == ReportSim ) ) {
                 // // 2, 3, 4
            std::string MaxOut; // Character for Max out string
            std::string MinOut; // Character for Min out string

            if (MaxValue == 0.0) {
                MaxOut = "0.0";
            } else {
                dtoa(MaxValue, state.dataOutputProcessor->s_WriteReportMeterData);
                MaxOut = std::string(state.dataOutputProcessor->s_WriteReportMeterData);
            }

            if (minValue == 0.0) {
                MinOut = "0.0";
            } else {
                dtoa(minValue, state.dataOutputProcessor->s_WriteReportMeterData);
                MinOut = std::string(state.dataOutputProcessor->s_WriteReportMeterData);
            }

            // Append the min and max strings with date information
            ProduceMinMaxString(MinOut, minValueDate, freq);
            ProduceMinMaxString(MaxOut, maxValueDate, freq);

            if (state.files.mtr.good()) {
                print(state.files.mtr, "{},{},{},{}\n", reportID, NumberOut, MinOut, MaxOut);
            }

            ++state.dataGlobal->StdMeterRecordCount;
            if (state.files.eso.good() && !meterOnlyFlag) {
                print(state.files.eso, "{},{},{},{}\n", reportID, NumberOut, MinOut, MaxOut);
                ++state.dataGlobal->StdOutputRecordCount;
            }
        }
    }

    void WriteNumericData(EnergyPlusData &state,
                          int const reportID,           // The variable's reporting ID
                          Real64 const repValue         // The variable's value
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mark Adams
        //       DATE WRITTEN   May 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE:
        // This subroutine writes real data to the output files and
        // SQL database.
        // This is a refactor of WriteRealData.
        //
        // Much of the code here was an included in earlier versions
        // of the UpdateDataandReport subroutine. The code was moved to facilitate
        // easier maintenance and writing of data to the SQL database.

        if (state.dataSysVars->UpdateDataDuringWarmupExternalInterface && !state.dataSysVars->ReportDuringWarmup) return;

        dtoa(repValue, state.dataOutputProcessor->s_WriteNumericData);

        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteReportDataRecord(reportID, repValue);
        }

        if (state.files.eso.good()) {
            print<FormatSyntax::FMT>(state.files.eso, "{},{}\n", reportID, state.dataOutputProcessor->s_WriteNumericData);
        }
    }

    void WriteNumericData(EnergyPlusData &state,
                          int const reportID,           // The variable's reporting ID
                          int32_t const repValue        // The variable's value
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mark Adams
        //       DATE WRITTEN   May 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE:
        // This subroutine writes real data to the output files and
        // SQL database.
        // This is a refactor of WriteIntegerData.
        //
        // Much of the code here was an included in earlier versions
        // of the UpdateDataandReport subroutine. The code was moved to facilitate
        // easier maintenance and writing of data to the SQL database.

        //        i32toa(repValue, state.dataOutputProcessor->s_WriteNumericData);

        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteReportDataRecord(reportID, repValue);
        }

        if (state.files.eso.good()) {
            print<FormatSyntax::FMT>(state.files.eso, "{},{}\n", reportID, fmt::format_int(repValue).c_str());
        }
    }

    void WriteIntegerVariableOutput(EnergyPlusData &state,
                                    OutVarInt *intVar,           // Integer variable to write out
                                    ReportFreq const reportFreq // The report type (i.e., the reporting interval)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   August 2008
        //       MODIFIED       April 2011; Linda Lawrie, December 2017; Jason DeGraw
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes integer report variable data to the output file and
        // SQL database. Much of the code here was an included in earlier versions
        // of the UpdateDataandReport subroutine. The code was moved to facilitate
        // easier maintenance and writing of data to the SQL database.

        if (state.dataSysVars->UpdateDataDuringWarmupExternalInterface && !state.dataSysVars->ReportDuringWarmup) return;

        if (intVar->Report && intVar->freq == reportFreq && intVar->Stored) {
            if (intVar->NumStored > 0.0) {
                WriteReportIntegerData(state,
                                       intVar->ReportID,
                                       intVar->StoreValue,
                                       intVar->storeType,
                                       intVar->NumStored,
                                       intVar->freq,
                                       intVar->MinValue,
                                       intVar->minValueDate,
                                       intVar->MaxValue,
                                       intVar->maxValueDate);
                ++state.dataGlobal->StdOutputRecordCount;
            }

            intVar->StoreValue = 0.0;
            intVar->NumStored = 0.0;
            intVar->MinValue = IMinSetValue;
            intVar->MaxValue = IMaxSetValue;
            intVar->Stored = false;
        }
    }

    void WriteReportIntegerData(EnergyPlusData &state,
                                int const reportID,                         // The variable's reporting ID
                                Real64 const repValue,                      // The variable's value
                                StoreType const storeType,                  // Type of item (averaged or summed)
                                Real64 const numOfItemsStored,              // The number of items (hours or timesteps) of data stored
                                ReportFreq const freq, // The reporting interval (e.g., monthly)
                                int const minValue,                         // The variable's minimum value during the reporting interval
                                int const minValueDate,                     // The date the minimum value occurred
                                int const MaxValue,                         // The variable's maximum value during the reporting interval
                                int const maxValueDate                      // The date the maximum value occurred
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   July 2008
        //       MODIFIED       April 2011; Linda Lawrie
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes averaged integer data to the output files and
        // SQL database. It supports the WriteIntegerVariableOutput subroutine.
        // Much of the code here was an included in earlier versions
        // of the UpdateDataandReport subroutine. The code was moved to facilitate
        // easier maintenance and writing of data to the SQL database.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string NumberOut; // Character for producing "number out"
        std::string MaxOut;    // Character for Max out string
        std::string MinOut;    // Character for Min out string
        Real64 rmaxValue;
        Real64 rminValue;
        Real64 repVal; // The variable's value
        auto &rf = state.dataResultsFramework->resultsFramework;
        
        repVal = repValue;
        if (storeType == StoreType::Averaged) {
            repVal /= numOfItemsStored;
        }
        if (repValue == 0.0) {
            NumberOut = "0.0";
        } else {
            NumberOut = format("{:f}", repVal);
        }

        // Append the min and max strings with date information
        MinOut = fmt::to_string(minValue);
        MaxOut = fmt::to_string(MaxValue);
        ProduceMinMaxString(MinOut, minValueDate, freq);
        ProduceMinMaxString(MaxOut, maxValueDate, freq);

        if (rf->timeSeriesEnabled() &&
            (freq == ReportFreq::Day || freq == ReportFreq::Month || freq == ReportFreq::Year || freq == ReportFreq::Simulation)) {
            // add to daily TS data store
            rf->freqTSData[(int)freq].pushVariableValue(reportID, repVal);
        }

        rminValue = minValue;
        rmaxValue = MaxValue;
        if (state.dataSQLiteProcedures->sqlite) {
            state.dataSQLiteProcedures->sqlite->createSQLiteReportDataRecord(
                reportID, repVal, (int)freq, rminValue, minValueDate, rmaxValue, maxValueDate);
        }

        if ((freq == ReportFreq::EachCall) || (freq == ReportFreq::TimeStep) || (freq == ReportFreq::Hour)) { // -1, 0, 1
            if (state.files.eso.good()) {
                print(state.files.eso, "{},{}\n", reportID, NumberOut);
            }
        } else { // if ( ( reportingInterval == ReportDaily ) || ( reportingInterval == ReportMonthly ) || ( reportingInterval == ReportSim ) ) {
                 // // 2, 3, 4
            if (state.files.eso.good()) {
                print(state.files.eso, "{},{},{},{}\n", reportID, NumberOut, MinOut, MaxOut);
            }
        }
    }

    int DetermineIndexGroupKeyFromMeterName([[maybe_unused]] EnergyPlusData &state, std::string const &meterName) // the meter name
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   May 2009

        // PURPOSE OF THIS FUNCTION:
        // This function attemps to guess determine how a meter variable should be
        // grouped.  It does this by parsing the meter name and then assigns a
        // indexGroupKey based on the name

        // Facility indices are in the 100s
        if (has(meterName, "Electricity:Facility")) {
            return 100;
        } else if (has(meterName, "NaturalGas:Facility")) {
            return 101;
        } else if (has(meterName, "DistricHeating:Facility")) {
            return 102;
        } else if (has(meterName, "DistricCooling:Facility")) {
            return 103;
        } else if (has(meterName, "ElectricityNet:Facility")) {
            return 104;

            // Building indices are in the 200s
        } else if (has(meterName, "Electricity:Building")) {
            return 201;
        } else if (has(meterName, "NaturalGas:Building")) {
            return 202;

            // HVAC indices are in the 300s
        } else if (has(meterName, "Electricity:HVAC")) {
            return 301;

            // InteriorLights:Electricity:Zone indices are in the 500s
        } else if (has(meterName, "InteriorLights:Electricity:Zone")) {
            return 501;

            // InteriorLights:Electricity indices are in the 400s
        } else if (has(meterName, "InteriorLights:Electricity")) {
            return 401;

            // Unknown items have negative indices
        } else {
            return -11;
        }

        return -1;
    }

    std::string DetermineIndexGroupFromMeterGroup(Meter const *meter) // the meter
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function attemps to determine how a meter variable should be
        // grouped.  It does this by parsing the meter group

        // Return value
        std::string indexGroup;

        if (len(meter->Group) > 0) {
            indexGroup = meter->Group;
        } else {
            indexGroup = "Facility";
        }

        if (meter->resource != Constant::eResource::Invalid) {
            indexGroup += format(":{}", Constant::eResourceNames[(int)meter->resource]);
        }

        if (len(meter->EndUse) > 0) {
            indexGroup += ":" + meter->EndUse;
        }

        if (len(meter->EndUseSub) > 0) {
            indexGroup += ":" + meter->EndUseSub;
        }

        return indexGroup;
    }

    void SetInternalVariableValue(EnergyPlusData &state,
                                  OutputProcessor::VariableType const varType, // 1=integer, 2=real, 3=meter
                                  int const keyVarIndex,                       // Array index
                                  Real64 const SetRealVal,                     // real value to set, if type is real or meter
                                  int const SetIntVal                          // integer value to set if type is integer
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2012

        // PURPOSE OF THIS SUBROUTINE:
        // This is a simple set routine for output pointers
        // It is intended for special use to reinitializations those pointers used for EMS sensors

        // METHODOLOGY EMPLOYED:
        // given a variable type and variable index,
        // assign the pointers the values passed in.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &op = state.dataOutputProcessor;

        if (varType == VariableType::Integer) {
            OutVarInt *varInt = dynamic_cast<OutVarInt *>(op->outVars[keyVarIndex]);
            assert(varInt != nullptr);
            *varInt->Which = SetIntVal;
        } else if (varType == VariableType::Real) {
            OutVarReal *varReal = dynamic_cast<OutVarReal *>(op->outVars[keyVarIndex]);
            assert(varReal != nullptr);
            *varReal->Which = SetRealVal;
        } else if (varType == VariableType::Meter) {
            op->meters[keyVarIndex]->CurTSValue = SetRealVal;
        }
    }

    // returns the unit string for a DDVariableTypes item and custom string when customEMS is used
    std::string unitStringFromDDitem([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int const ddItemPtr // index provided for DDVariableTypes
    )
    {
            assert(false); return "";
    }


} // namespace OutputProcessor

// TODO: Probably move these to a different location

void SetupOutputVariable(EnergyPlusData &state,
                         std::string_view const name,                            // String Name of variable (with units)
                         Constant::Units const units,                            // Actual units corresponding to the actual variable
                         Real64 &ActualVariable,                                 // Actual Variable, used to set up pointer
                         OutputProcessor::SOVTimeStepType sovTimeStep,     // Zone, HeatBalance=1, HVAC, System, Plant=2
                         OutputProcessor::SOVStoreType sovStore,           // State, Average=1, NonState, Sum=2
                         std::string const &key,                                 // Associated Key for this variable
                         OutputProcessor::ReportFreq freq,                       // Internal use -- causes reporting at this frequency
                         Constant::eResource resource,                           // Meter Resource Type (Electricity, Gas, etc)
                         std::string_view const EndUse,                          // Meter End Use Key (Lights, Heating, Cooling, etc)
                         std::string_view const EndUseSub,                       // Meter End Use Sub Key (General Lights, Task Lights, etc)
                         std::string_view const Group,                           // Meter Super Group Key (Building, System, Plant)
                         std::string_view const Zone,                            // Meter Zone Key (zone name)
                         int const ZoneMult,                                     // Zone Multiplier, defaults to 1
                         int const ZoneListMult,                                 // Zone List Multiplier, defaults to 1
                         int const indexGroupKey,                                // Group identifier for SQL output
                         std::string_view const customUnitName,                  // the custom name for the units from EMS definition of units
                         std::string_view const SpaceType                        // Space type (applicable for Building group only)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 1998
    //       MODIFIED       January 2001; Implement Meters
    //                      August 2008; Implement SQL output
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets up the variable data structure that will be used
    // to track values of the output variables of EnergyPlus.

    // METHODOLOGY EMPLOYED:
    // Pointers (as pointers), pointers (as indices), and lots of other KEWL data stuff.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string zoneName;     // Will hold value of ZoneKey
    std::string spaceType;    // Will hold value of SpaceType

    using namespace OutputProcessor;

    auto &op = state.dataOutputProcessor;

    if (!op->OutputInitialized) InitializeOutput(state);

    std::vector<int>reqVarList;
    
    // Determine whether to Report or not
    CheckReportVariable(state, name, key, reqVarList);

    // DataOutputs::OutputVariablesForSimulation is case-insensitive

    // Is this redundant with CheckReportVariable?
    bool const ThisOneOnTheList = DataOutputs::FindItemInVariableList(state, key, name);

    TimeStepType timeStep = sovTimeStep2TimeStep[(int)sovTimeStep];
    StoreType store = sovStoreType2StoreType[(int)sovStore];

    bool OnMeter = (resource != Constant::eResource::Invalid) ||
            (!EndUse.empty()) || (!EndUseSub.empty()) || (!Group.empty()) || (!Zone.empty()) || (!SpaceType.empty());

    if (OnMeter && store == StoreType::Averaged) {
        ShowSevereError(state, "Meters can only be \"Summed\" variables");
        ShowContinueError(state, fmt::format("..reference variable={}:{}", key, name));
        OnMeter = false;
    }

    int ddOutVarNum = AddDDOutVar(state, name, timeStep, store, VariableType::Real, units, customUnitName);
    auto *ddOutVar = op->ddOutVars[ddOutVarNum];
    
    ++op->NumOfRVariable_Setup;

    // If we add any output variables here at all, the first one will be at this index
    int firstAddedOutVarNum = (int)op->outVars.size();

    op->NumTotalRVariable += reqVarList.size();
    if (store == StoreType::Summed) ++op->NumOfRVariable_Sum;
    if (OnMeter) ++op->NumOfRVariable_Meter;
    
    if (!OnMeter && !ThisOneOnTheList) return;

    for (int reqVarNum : reqVarList) {
            
        ++op->NumOfRVariable;

        OutVarReal *var = new OutVarReal;
        op->outVars.push_back(var);

        // Link this keyed variable to the dictionary entry
        ddOutVar->keyOutVarNums.push_back(op->outVars.size() - 1);
        var->ddVarNum = ddOutVarNum;
        
        var->timeStepType = timeStep;
        var->storeType = store;
        var->name = name;
        var->nameUC = UtilityRoutines::MakeUPPERCase(var->name);
        var->keyUC = UtilityRoutines::MakeUPPERCase(key);
        var->keyColonName = fmt::format("{}:{}", key, name);
        var->keyColonNameUC = UtilityRoutines::MakeUPPERCase(var->keyColonName);
        var->units = units;
        if (units == Constant::Units::customEMS) var->unitNameCustomEMS = customUnitName;
        var->ReportID = ++op->CurrentReportNumber;
        var->Which = &ActualVariable;
        var->ZoneMult = ZoneMult;
        var->ZoneListMult = ZoneListMult;

        var->Report = true;

        // freq != ReportFreq::Hour
        if (freq != ReportFreq::Hour) {
            var->freq = freq;
            var->SchedPtr = 0;
        } else {
            var->freq = op->reqVars[reqVarNum]->freq;
            var->SchedPtr = op->reqVars[reqVarNum]->SchedPtr;
        }

        WriteReportVariableDictionaryItem(state,
                                          var->freq,
                                          var->storeType,
                                          var->ReportID,
                                          indexGroupKey,
                                          std::string(sovTimeStepTypeStrings[(int)sovTimeStep]),
                                          key,
                                          name,
                                          var->timeStepType,
                                          var->units,
                                          var->unitNameCustomEMS,
                                          (var->SchedPtr != 0) ? op->reqVars[reqVarNum]->SchedName : "");
    }

    if (OnMeter) {
        AttachMeters(state, units, resource, EndUse, EndUseSub, Group, zoneName, spaceType, firstAddedOutVarNum);
    }
}

void SetupOutputVariable(EnergyPlusData &state,
                         std::string_view const name,                    // String Name of variable
                         Constant::Units const units,               // Actual units corresponding to the actual variable
                         int &ActualVariable,                                    // Actual Variable, used to set up pointer
                         OutputProcessor::SOVTimeStepType sovTimeStepType, // Zone, HeatBalance=1, HVAC, System, Plant=2
                         OutputProcessor::SOVStoreType sovStoreType,    // State, Average=1, NonState, Sum=2
                         std::string const &key,                      // Associated Key for this variable
                         OutputProcessor::ReportFreq freq,                       // Internal use -- causes reporting at this freqency
                         int const indexGroupKey                                 // Group identifier for SQL output
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 1998
    //       MODIFIED       August 2008; Added SQL output capability
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets up the variable data structure that will be used
    // to track values of the output variables of EnergyPlus.

    // METHODOLOGY EMPLOYED:
    // Pointers (as pointers), pointers (as indices), and lots of other KEWL data stuff.

    // Using/Aliasing
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    auto &op = state.dataOutputProcessor;

    if (!op->OutputInitialized) InitializeOutput(state);

    // Determine whether to Report or not
    std::vector<int> reqVarNums;
    CheckReportVariable(state, name, key, reqVarNums);

    TimeStepType timeStepType = sovTimeStep2TimeStep[(int)sovTimeStepType];
    StoreType storeType = sovStoreType2StoreType[(int)sovStoreType];

    // DataOutputs::OutputVariablesForSimulation is case-insentitive
    bool ThisOneOnTheList = DataOutputs::FindItemInVariableList(state, key, name);
    if (!ThisOneOnTheList)
        return;
    
    int ddOutVarNum = AddDDOutVar(state, name, timeStepType, storeType, VariableType::Integer, units);
    auto *ddOutVar = op->ddOutVars[ddOutVarNum];
    
    ++op->NumOfIVariable_Setup;
    if (storeType == StoreType::Summed) {
        ++op->NumOfIVariable_Sum;
    }

    for (int Loop = 0; Loop < (int)reqVarNums.size(); ++Loop) {

        ++op->NumTotalIVariable;
        ++op->NumOfIVariable;
        
        OutVarInt *var = new OutVarInt;
        op->outVars.push_back(var);
        // Add to ddVar key list
        ddOutVar->keyOutVarNums.push_back(op->outVars.size() - 1);
        
        var->varType = VariableType::Integer;
        var->timeStepType = timeStepType;
        var->storeType = storeType;
        var->name = name;
        var->nameUC = UtilityRoutines::MakeUPPERCase(var->name);
        var->keyUC = UtilityRoutines::MakeUPPERCase(key);
        var->keyColonName = fmt::format("{}:{}", key, name);
        var->keyColonNameUC = UtilityRoutines::MakeUPPERCase(var->keyColonName);
        var->units = units;
        var->ReportID = ++op->CurrentReportNumber;
        var->Which = &ActualVariable;

        var->Report = true;

        if (freq != ReportFreq::Hour) {
            var->freq = freq;
            var->SchedPtr = 0;
        } else {
            var->freq = op->reqVars[reqVarNums[Loop]]->freq;
            var->SchedPtr = op->reqVars[reqVarNums[Loop]]->SchedPtr;
        }

        WriteReportVariableDictionaryItem(state,
                                          var->freq,
                                          var->storeType,
                                          var->ReportID,
                                          indexGroupKey,
                                          std::string(sovTimeStepTypeStrings[(int)sovTimeStepType]),
                                          key,
                                          name, 
                                          var->timeStepType,
                                          var->units,
                                          (var->SchedPtr != 0) ? op->reqVars[reqVarNums[Loop]]->SchedName : "");
    }
}

void UpdateDataandReport(EnergyPlusData &state, OutputProcessor::TimeStepType const t_TimeStepTypeKey) // What kind of data to update (Zone, HVAC)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 1998
    //       MODIFIED       January 2001; Resolution integrated at the Zone TimeStep intervals
    //       MODIFIED       August 2008; Added SQL output capability
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine writes the actual report variable (for user requested
    // Report Variables) strings to the standard output file.

    // Using/Aliasing
    using namespace OutputProcessor;
    using General::EncodeMonDayHrMin;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool TimePrint(true);        // True if the time needs to be printed
    bool EndTimeStepFlag(false); // True when it's the end of the Zone Time Step
    auto &op = state.dataOutputProcessor;
    auto &rf = state.dataResultsFramework->resultsFramework;

    if (t_TimeStepTypeKey != TimeStepType::Zone && t_TimeStepTypeKey != TimeStepType::System) {
        ShowFatalError(state, "Invalid reporting requested -- UpdateDataAndReport");
    }

    // Basic record keeping and report out if "detailed"
    Real64 StartMinute = op->TimeValue[(int)t_TimeStepTypeKey].CurMinute; // StartMinute for UpdateData call
    op->TimeValue[(int)t_TimeStepTypeKey].CurMinute += (*op->TimeValue[(int)t_TimeStepTypeKey].TimeStep) * 60.0;
    if (t_TimeStepTypeKey == TimeStepType::System &&
        (op->TimeValue[(int)TimeStepType::System].CurMinute == op->TimeValue[(int)TimeStepType::Zone].CurMinute)) {
        EndTimeStepFlag = true;
    } else if (t_TimeStepTypeKey == TimeStepType::Zone) {
        EndTimeStepFlag = true;
    } else {
        EndTimeStepFlag = false;
    }
    Real64 MinuteNow = op->TimeValue[(int)t_TimeStepTypeKey].CurMinute; // What minute it is now

    int MDHM; // Month,Day,Hour,Minute
    EncodeMonDayHrMin(MDHM, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, int(MinuteNow));
    TimePrint = true;

    Real64 rxTime = (MinuteNow - StartMinute) /
                    double(state.dataGlobal->MinutesPerTimeStep); // (MinuteNow-StartMinute)/REAL(MinutesPerTimeStep,r64) - for execution time

    if (rf->timeSeriesEnabled()) {
        // R and I data frames for TimeStepType::TimeStepZone
        if (!rf->detailedTSData[(int)t_TimeStepTypeKey].variablesScanned()) {
            rf->initializeTSDataFrame(ReportFreq::EachCall, op->outVars, t_TimeStepTypeKey);
        }
    }

    if (rf->timeSeriesEnabled()) {
        rf->detailedTSData[(int)t_TimeStepTypeKey].newRow(state.dataEnvrn->Month,
                                                            state.dataEnvrn->DayOfMonth,
                                                            state.dataGlobal->HourOfDay,
                                                            op->TimeValue[(int)t_TimeStepTypeKey].CurMinute,
                                                            state.dataGlobal->CalendarYear);
    }

    // Main "Record Keeping" Loops for R and I variables
    for (auto *var : op->outVars) {
        if (var->varType != VariableType::Real) continue;
        if (var->timeStepType != t_TimeStepTypeKey) continue;

        OutVarReal *rVar = dynamic_cast<OutVarReal *>(var);
        assert(rVar != nullptr);

        rVar->Stored = true;
        if (rVar->storeType == StoreType::Averaged) {
            Real64 CurVal = (*rVar->Which) * rxTime;
            //        CALL SetMinMax(RVar%Which,MDHM,RVar%MaxValue,RVar%maxValueDate,RVar%MinValue,RVar%minValueDate)
            if ((*rVar->Which) > rVar->MaxValue) {
                rVar->MaxValue = (*rVar->Which);
                rVar->maxValueDate = MDHM;
            }
            if ((*rVar->Which) < rVar->MinValue) {
                rVar->MinValue = (*rVar->Which);
                rVar->minValueDate = MDHM;
            }
            rVar->TSValue += CurVal;
            rVar->EITSValue = rVar->TSValue; // CR - 8481 fix - 09/06/2011
        } else {
            //        CurVal=RVar%Which
            if ((*rVar->Which) > rVar->MaxValue) {
                rVar->MaxValue = (*rVar->Which);
                rVar->maxValueDate = MDHM;
            }
            if ((*rVar->Which) < rVar->MinValue) {
                rVar->MinValue = (*rVar->Which);
                rVar->minValueDate = MDHM;
            }
            rVar->TSValue += (*rVar->Which);
            rVar->EITSValue = rVar->TSValue; // CR - 8481 fix - 09/06/2011
        }

        // End of "record keeping"  Report if applicable
        if (!rVar->Report) continue;
        bool ReportNow = true;
        if (rVar->SchedPtr > 0) ReportNow = (GetCurrentScheduleValue(state, rVar->SchedPtr) != 0.0); // SetReportNow(RVar%SchedPtr)
        if (!ReportNow) continue;
        rVar->tsStored = true;
        if (!rVar->thisTSStored) {
            ++rVar->thisTSCount;
            rVar->thisTSStored = true;
        }

        if (rVar->freq == ReportFreq::EachCall) {
            if (TimePrint) {
                if (op->LHourP != state.dataGlobal->HourOfDay || std::abs(op->LStartMin - StartMinute) > 0.001 ||
                    std::abs(op->LEndMin - op->TimeValue[(int)t_TimeStepTypeKey].CurMinute) > 0.001) {
                    int CurDayType = state.dataEnvrn->DayOfWeek;
                    if (state.dataEnvrn->HolidayIndex > 0) {
                        CurDayType = state.dataEnvrn->HolidayIndex;
                    }
                    WriteTimeStampFormatData(state,
                                             state.files.eso,
                                             ReportFreq::EachCall,
                                             op->freqStampReportNums[(int)ReportFreq::TimeStep],
                                             state.dataGlobal->DayOfSimChr,
                                             true,
                                             state.dataEnvrn->Month,
                                             state.dataEnvrn->DayOfMonth,
                                             state.dataGlobal->HourOfDay,
                                             op->TimeValue[(int)t_TimeStepTypeKey].CurMinute,
                                             StartMinute,
                                             state.dataEnvrn->DSTIndicator,
                                             ScheduleManager::dayTypeNames[CurDayType]);
                    op->LHourP = state.dataGlobal->HourOfDay;
                    op->LStartMin = StartMinute;
                    op->LEndMin = op->TimeValue[(int)t_TimeStepTypeKey].CurMinute;
                }
                TimePrint = false;
            }
            WriteNumericData(state, rVar->ReportID, *rVar->Which);
            ++state.dataGlobal->StdOutputRecordCount;

            if (rf->timeSeriesEnabled()) {
                rf->detailedTSData[(int)t_TimeStepTypeKey].pushVariableValue(rVar->ReportID, *rVar->Which);
            }
        }
    }

    for (auto *var : op->outVars) {
        if (var->varType != VariableType::Integer) continue;
        if (var->timeStepType != t_TimeStepTypeKey) continue;

        // Act on the IVariables variable
        auto *iVar = dynamic_cast<OutVarInt *>(var);
        assert(iVar != nullptr);
        
        iVar->Stored = true;
        //      ICurVal=IVar%Which
        if (iVar->storeType == StoreType::Averaged) {
            Real64 ICurVal = (*iVar->Which) * rxTime;
            iVar->TSValue += ICurVal;
            iVar->EITSValue = iVar->TSValue; // CR - 8481 fix - 09/06/2011
            if (nint(ICurVal) > iVar->MaxValue) {
                iVar->MaxValue = nint(ICurVal); // Record keeping for date and time go here too
                iVar->maxValueDate = MDHM;      //+ TimeValue.at(t_TimeStepTypeKey)%TimeStep
            }
            if (nint(ICurVal) < iVar->MinValue) {
                iVar->MinValue = nint(ICurVal);
                iVar->minValueDate = MDHM; //+ TimeValue.at(t_TimeStepTypeKey)%TimeStep
            }
        } else {
            if ((*iVar->Which) > iVar->MaxValue) {
                iVar->MaxValue = (*iVar->Which); // Record keeping for date and time go here too
                iVar->maxValueDate = MDHM;      //+ TimeValue(TimeStepType)%TimeStep
            }
            if ((*iVar->Which) < iVar->MinValue) {
                iVar->MinValue = (*iVar->Which);
                iVar->minValueDate = MDHM; //+ TimeValue(TimeStepType)%TimeStep
            }
            iVar->TSValue += (*iVar->Which);
            iVar->EITSValue = iVar->TSValue; // CR - 8481 fix - 09/06/2011
        }

        if (!iVar->Report) continue;
        bool ReportNow = true;
        if (iVar->SchedPtr > 0) ReportNow = (GetCurrentScheduleValue(state, iVar->SchedPtr) != 0.0); // SetReportNow(IVar%SchedPtr)
        if (!ReportNow) continue;
        iVar->tsStored = true;
        if (!iVar->thisTSStored) {
            ++iVar->thisTSCount;
            iVar->thisTSStored = true;
        }

        if (iVar->freq == ReportFreq::EachCall) {
            if (TimePrint) {
                if (op->LHourP != state.dataGlobal->HourOfDay || std::abs(op->LStartMin - StartMinute) > 0.001 ||
                    std::abs(op->LEndMin - op->TimeValue[(int)t_TimeStepTypeKey].CurMinute) > 0.001) {
                    int CurDayType = state.dataEnvrn->DayOfWeek;
                    if (state.dataEnvrn->HolidayIndex > 0) {
                        CurDayType = state.dataEnvrn->HolidayIndex;
                    }
                    WriteTimeStampFormatData(state,
                                             state.files.eso,
                                             ReportFreq::EachCall,
                                             op->freqStampReportNums[(int)ReportFreq::TimeStep],
                                             state.dataGlobal->DayOfSimChr,
                                             true,
                                             state.dataEnvrn->Month,
                                             state.dataEnvrn->DayOfMonth,
                                             state.dataGlobal->HourOfDay,
                                             op->TimeValue[(int)t_TimeStepTypeKey].CurMinute,
                                             StartMinute,
                                             state.dataEnvrn->DSTIndicator,
                                             ScheduleManager::dayTypeNames[CurDayType]);
                    op->LHourP = state.dataGlobal->HourOfDay;
                    op->LStartMin = StartMinute;
                    op->LEndMin = op->TimeValue[(int)t_TimeStepTypeKey].CurMinute;
                }
                TimePrint = false;
            }
            // only time integer vars actual report as integer only is "detailed"
            WriteNumericData(state, iVar->ReportID, *iVar->Which);
            ++state.dataGlobal->StdOutputRecordCount;

            if (rf->timeSeriesEnabled()) {
                rf->detailedTSData[(int)t_TimeStepTypeKey].pushVariableValue(iVar->ReportID, *iVar->Which);
            }
        }
    }

    if (t_TimeStepTypeKey == TimeStepType::System) return; // All other stuff happens at the "zone" time step call to this routine.

    // TimeStep Block (Report on Zone TimeStep)

    if (EndTimeStepFlag) {
        if (rf->timeSeriesEnabled()) {
            if (!rf->freqTSData[(int)ReportFreq::TimeStep].variablesScanned()) {
                rf->initializeTSDataFrame(ReportFreq::TimeStep, op->outVars);
            }
            rf->freqTSData[(int)ReportFreq::TimeStep].newRow(state.dataEnvrn->Month,
                                                               state.dataEnvrn->DayOfMonth,
                                                               state.dataGlobal->HourOfDay,
                                                               op->TimeValue[(int)TimeStepType::Zone].CurMinute,
                                                               state.dataGlobal->CalendarYear);
        }

        for (TimeStepType thisTimeStepType : {TimeStepType::Zone, TimeStepType::System}) { // Zone, HVAC
            for (auto *var : op->outVars) {
                if (var->timeStepType != thisTimeStepType) continue;
                // auto *rVar = dynamic_cast<OutVarReal *>(var);
                // assert(rVar != nullptr);
                
                // Update meters on the TimeStep  (Zone)
                if (var->meterNums.size() == 0) continue;

                if (op->meterValues.empty()) continue; // Is this necessary?
                
                Real64 TimeStepValue = var->TSValue * var->ZoneMult * var->ZoneListMult;
                for (int meterNum : var->meterNums) { 
                    op->meterValues[meterNum] += TimeStepValue;
                }

                bool ReportNow = true;
                if (var->SchedPtr > 0) ReportNow = (GetCurrentScheduleValue(state, var->SchedPtr) != 0.0); // SetReportNow(RVar%SchedPtr)
                if (!ReportNow || !var->Report) {
                    var->TSValue = 0.0;
                }
                //        IF (RVar%StoreType == AveragedVar) THEN
                //          RVar%Value=RVar%Value+RVar%TSValue/NumOfTimeStepInHour
                //        ELSE
                var->Value += var->TSValue;
                //        ENDIF

                if (!ReportNow || !var->Report) continue;

                if (var->freq == ReportFreq::TimeStep) {
                    if (TimePrint) {
                        if (op->LHourP != state.dataGlobal->HourOfDay || std::abs(op->LStartMin - StartMinute) > 0.001 ||
                            std::abs(op->LEndMin - op->TimeValue[(int)thisTimeStepType].CurMinute) > 0.001) {
                            int CurDayType = state.dataEnvrn->DayOfWeek;
                            if (state.dataEnvrn->HolidayIndex > 0) {
                                CurDayType = state.dataEnvrn->HolidayIndex;
                            }
                            WriteTimeStampFormatData(state,
                                                     state.files.eso,
                                                     ReportFreq::EachCall,
                                                     op->freqStampReportNums[(int)ReportFreq::TimeStep],
                                                     state.dataGlobal->DayOfSimChr,
                                                     true,
                                                     state.dataEnvrn->Month,
                                                     state.dataEnvrn->DayOfMonth,
                                                     state.dataGlobal->HourOfDay,
                                                     op->TimeValue[(int)thisTimeStepType].CurMinute,
                                                     StartMinute,
                                                     state.dataEnvrn->DSTIndicator,
                                                     ScheduleManager::dayTypeNames[CurDayType]);
                            op->LHourP = state.dataGlobal->HourOfDay;
                            op->LStartMin = StartMinute;
                            op->LEndMin = op->TimeValue[(int)thisTimeStepType].CurMinute;
                        }
                        TimePrint = false;
                    } // if (TimePrint)

                    WriteNumericData(state, var->ReportID, var->TSValue);
                    ++state.dataGlobal->StdOutputRecordCount;

                    if (rf->timeSeriesEnabled()) {
                        rf->freqTSData[(int)ReportFreq::TimeStep].pushVariableValue(var->ReportID, var->TSValue);
                    }
                }
                var->TSValue = 0.0;
                var->thisTSStored = false;
            } 
        }     // Index Type (Zone or HVAC)

        UpdateMeters(state, MDHM);

        ReportTSMeters(state, StartMinute, op->TimeValue[(int)TimeStepType::Zone].CurMinute, TimePrint, TimePrint);

    } // TimeStep Block

    // Hour Block
    if (state.dataGlobal->EndHourFlag) {
        if (op->freqTrackingVariables[(int)ReportFreq::Hour]) {
            int CurDayType = state.dataEnvrn->DayOfWeek;
            if (state.dataEnvrn->HolidayIndex > 0) {
                CurDayType = state.dataEnvrn->HolidayIndex;
            }
            WriteTimeStampFormatData(state,
                                     state.files.eso,
                                     ReportFreq::Hour,
                                     op->freqStampReportNums[(int)ReportFreq::TimeStep],
                                     state.dataGlobal->DayOfSimChr,
                                     true,
                                     state.dataEnvrn->Month,
                                     state.dataEnvrn->DayOfMonth,
                                     state.dataGlobal->HourOfDay,
                                     _,
                                     _,
                                     state.dataEnvrn->DSTIndicator,
                                     ScheduleManager::dayTypeNames[CurDayType]);
            TimePrint = false;
        }

        if (rf->timeSeriesEnabled()) {
            if (!rf->freqTSData[(int)ReportFreq::Hour].variablesScanned()) {
                rf->initializeTSDataFrame(ReportFreq::Hour, op->outVars);
            }
            rf->freqTSData[(int)ReportFreq::Hour].newRow(state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0, state.dataGlobal->CalendarYear);
        }

        for (TimeStepType thisTimeStepType : {TimeStepType::Zone, TimeStepType::System}) { // Zone, HVAC
            op->TimeValue[(int)thisTimeStepType].CurMinute = 0.0;

            for (auto *var : op->outVars) {
                if (var->timeStepType != thisTimeStepType) continue;

                // auto *rVar = dynamic_cast<OutVarReal *>(var);
                // assert(rVar != nullptr);
                
                //        ReportNow=.TRUE.
                //        IF (RVar%SchedPtr > 0) &
                //          ReportNow=(GetCurrentScheduleValue(state, RVar%SchedPtr) /= 0.0)  !SetReportNow(RVar%SchedPtr)

                //        IF (ReportNow) THEN
                if (var->tsStored) {
                    if (var->storeType == StoreType::Averaged) {
                        var->Value /= double(var->thisTSCount);
                    }
                    if (var->Report && var->freq == ReportFreq::Hour && var->Stored) {
                        WriteNumericData(state, var->ReportID, var->Value);
                        ++state.dataGlobal->StdOutputRecordCount;
                        var->Stored = false;
                        // add time series value for hourly to data store
                        if (rf->timeSeriesEnabled()) {
                            rf->freqTSData[(int)ReportFreq::Hour].pushVariableValue(var->ReportID, var->Value);
                        }
                    }
                    var->StoreValue += var->Value;
                    ++var->NumStored;
                }
                var->tsStored = false;
                var->thisTSStored = false;
                var->thisTSCount = 0;
                var->Value = 0.0;
            } // Number of R Variables
        }     // thisTimeStepType (Zone or HVAC)

        ReportMeters(state, ReportFreq::Hour, TimePrint);
    } // Hour Block

    if (!state.dataGlobal->EndHourFlag) return;

    // Day Block
    if (state.dataGlobal->EndDayFlag) {
        if (op->freqTrackingVariables[(int)ReportFreq::Day]) {
            int CurDayType = state.dataEnvrn->DayOfWeek;
            if (state.dataEnvrn->HolidayIndex > 0) {
                CurDayType = state.dataEnvrn->HolidayIndex;
            }
            WriteTimeStampFormatData(state,
                                     state.files.eso,
                                     ReportFreq::Day,
                                     op->freqStampReportNums[(int)ReportFreq::Day],
                                     state.dataGlobal->DayOfSimChr,
                                     true,
                                     state.dataEnvrn->Month,
                                     state.dataEnvrn->DayOfMonth,
                                     _,
                                     _,
                                     _,
                                     state.dataEnvrn->DSTIndicator,
                                     ScheduleManager::dayTypeNames[CurDayType]);
            TimePrint = false;
        }
        if (rf->timeSeriesEnabled()) {
            if (!rf->freqTSData[(int)ReportFreq::Day].variablesScanned()) {
                rf->initializeTSDataFrame(ReportFreq::Day, op->outVars);
            }
            rf->freqTSData[(int)ReportFreq::Day].newRow(
                state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0, state.dataGlobal->CalendarYear);
        }

        op->NumHoursInMonth += 24;
        for (TimeStepType thisTimeStepType : {TimeStepType::Zone, TimeStepType::System}) { // Zone, HVAC
            for (auto *var : op->outVars) {
                if (var->timeStepType != thisTimeStepType) continue;

                if (var->varType == VariableType::Real) {
                    WriteRealVariableOutput(state, dynamic_cast<OutVarReal *>(var), ReportFreq::Day);
                } else if (var->varType == VariableType::Integer) {
                    WriteIntegerVariableOutput(state, dynamic_cast<OutVarInt *>(var), ReportFreq::Day);
                }
            } 
        }     // thisTimeStepType (Zone or HVAC)

        ReportMeters(state, ReportFreq::Day, TimePrint);

    } // Day Block

    // Only continue if EndDayFlag is set
    if (!state.dataGlobal->EndDayFlag) return;

    // Month Block
    if (state.dataEnvrn->EndMonthFlag || state.dataGlobal->EndEnvrnFlag) {
        if (op->freqTrackingVariables[(int)ReportFreq::Month]) {
            WriteTimeStampFormatData(state,
                                     state.files.eso,
                                     ReportFreq::Month,
                                     op->freqStampReportNums[(int)ReportFreq::Month],
                                     state.dataGlobal->DayOfSimChr,
                                     true,
                                     state.dataEnvrn->Month);
            TimePrint = false;
        }

        if (rf->timeSeriesEnabled()) {
            if (!rf->freqTSData[(int)ReportFreq::Month].variablesScanned()) {
                rf->initializeTSDataFrame(ReportFreq::Month, op->outVars);
            }
            rf->freqTSData[(int)ReportFreq::Month].newRow(
                state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0, state.dataGlobal->CalendarYear);
        }

        op->NumHoursInSim += op->NumHoursInMonth;
        state.dataEnvrn->EndMonthFlag = false;
        for (TimeStepType thisTimeStepType : {TimeStepType::Zone, TimeStepType::System}) { // Zone, HVAC
            for (auto *var : op->outVars) {
                if (var->timeStepType == thisTimeStepType) continue;

                if (var->varType == VariableType::Real) {
                    WriteRealVariableOutput(state, dynamic_cast<OutVarReal *>(var), ReportFreq::Month);
                } else if (var->varType == VariableType::Integer) {
                    WriteIntegerVariableOutput(state, dynamic_cast<OutVarInt *>(var), ReportFreq::Month);
                }
            } // for (var)
        }  // thisTimeStepType (Zone, HVAC)

        ReportMeters(state, ReportFreq::Month, TimePrint);

        op->NumHoursInMonth = 0;
    } // Month Block

    // Sim/Environment Block
    if (state.dataGlobal->EndEnvrnFlag) {
        if (op->freqTrackingVariables[(int)ReportFreq::Simulation]) {
            WriteTimeStampFormatData(state,
                                     state.files.eso,
                                     ReportFreq::Simulation,
                                     op->freqStampReportNums[(int)ReportFreq::Simulation],
                                     state.dataGlobal->DayOfSimChr,
                                     true);
            TimePrint = false;
        }

        if (rf->timeSeriesEnabled()) {
            if (!rf->freqTSData[(int)ReportFreq::Simulation].variablesScanned()) {
                rf->initializeTSDataFrame(ReportFreq::Simulation, op->outVars);
            }
            rf->freqTSData[(int)ReportFreq::Simulation].newRow(state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0, state.dataGlobal->CalendarYear);
        }
        for (TimeStepType thisTimeStepType : {TimeStepType::Zone, TimeStepType::System}) { // Zone, HVAC
            for (auto *var : op->outVars) {
                if (var->timeStepType != thisTimeStepType) continue;
                if (var->varType == VariableType::Real) {
                    WriteRealVariableOutput(state, dynamic_cast<OutVarReal *>(var), ReportFreq::Simulation);
                } else if (var->varType == VariableType::Integer) {
                    WriteIntegerVariableOutput(state, dynamic_cast<OutVarInt *>(var), ReportFreq::Simulation);
                }
            } // Number of I Variables
        }     // thisTimeStepType (Zone, HVAC)

        ReportMeters(state, ReportFreq::Simulation, TimePrint);

        op->NumHoursInSim = 0;
    }

    // Yearly Block
    if (state.dataEnvrn->EndYearFlag) {
        if (op->freqTrackingVariables[(int)ReportFreq::Year]) {
            WriteYearlyTimeStamp(state, state.files.eso, op->freqStampReportNums[(int)ReportFreq::Year], state.dataGlobal->CalendarYearChr, true);
            TimePrint = false;
        }
        if (rf->timeSeriesEnabled()) {
            if (!rf->freqTSData[(int)ReportFreq::Year].variablesScanned()) {
                rf->initializeTSDataFrame(ReportFreq::Year, op->outVars);
            }
            rf->freqTSData[(int)ReportFreq::Year].newRow(
                state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0, state.dataGlobal->CalendarYear);
        }
        for (TimeStepType thisTimeStepType : {TimeStepType::Zone, TimeStepType::System}) { // Zone, HVAC
            for (auto *var : op->outVars) {
                if (var->timeStepType != thisTimeStepType) continue;
                if (var->varType == VariableType::Real) {
                    WriteRealVariableOutput(state, dynamic_cast<OutVarReal *>(var), ReportFreq::Year);
                } else if (var->varType == VariableType::Integer) {
                    WriteIntegerVariableOutput(state, dynamic_cast<OutVarInt *>(var), ReportFreq::Year);
                }
            } // Number of I Variables
        }     // thisTimeStepType (Zone, HVAC)

        ReportMeters(state, ReportFreq::Year, TimePrint);

        state.dataGlobal->CalendarYear += 1;
        state.dataGlobal->CalendarYearChr = fmt::to_string(state.dataGlobal->CalendarYear);
    }
}

void GenOutputVariablesAuditReport(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2000

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine reports (to the .err file) any report variables
    // which were requested but not "setup" during the run.  These will
    // either be items that were not used in the IDF file or misspellings
    // of report variable names.

    // METHODOLOGY EMPLOYED:
    // Use flagged data structure in OutputProcessor.

    // Using/Aliasing
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    auto &op = state.dataOutputProcessor;

    for (auto *reqVar : op->reqVars) {
        if (reqVar->Used) continue;
        if (reqVar->key.empty()) reqVar->key = "*";
        if (has(reqVar->name, "OPAQUE SURFACE INSIDE FACE CONDUCTION") && !state.dataGlobal->DisplayAdvancedReportVariables &&
            !state.dataOutputProcessor->OpaqSurfWarned) {
            ShowWarningError(state, R"(Variables containing "Opaque Surface Inside Face Conduction" are now "advanced" variables.)");
            ShowContinueError(state, "You must enter the \"Output:Diagnostics,DisplayAdvancedReportVariables;\" statement to view.");
            ShowContinueError(state, "First, though, read cautionary statements in the \"InputOutputReference\" document.");
            state.dataOutputProcessor->OpaqSurfWarned = true;
        }
        if (!state.dataOutputProcessor->Rept) {
            ShowWarningError(state, "The following Report Variables were requested but not generated -- check.rdd file");
            ShowContinueError(state, "Either the IDF did not contain these elements, the variable name is misspelled,");
            ShowContinueError(state,
                              "or the requested variable is an advanced output which requires Output : Diagnostics, DisplayAdvancedReportVariables;");
            state.dataOutputProcessor->Rept = true;
        }
        ShowMessage(state, format("Key={}, VarName={}, Frequency={}", reqVar->key, reqVar->name, reportFreqNames[(int)reqVar->freq]));
    }
}

void UpdateMeterReporting(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   January 2001
    //       MODIFIED       February 2007 -- add cumulative meter reporting
    //                      January 2012 -- add predefined tabular meter reporting
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is called at the end of the first HVAC iteration and
    // sets up the reporting for the Energy Meters.  It also may show a fatal error
    // if errors occurred during initial SetupOutputVariable processing.  It "gets"
    // the Report Meter input:
    // Report Meter,
    //        \memo Meters requested here show up on eplusout.eso and eplusout.mtr
    //   A1 , \field Meter_Name
    //        \required-field
    //        \note Form is EnergyUseType:..., e.g. Electricity:* for all Electricity meters
    //        \note or EndUse:..., e.g. InteriorLights:* for all interior lights
    //        \note Report MeterFileOnly puts results on the eplusout.mtr file only
    //   A2 ; \field Reporting_Frequency
    //        \type choice
    //        \key timestep
    //        \note timestep refers to the zone timestep/timestep in hour value
    //        \note runperiod, environment, and annual are the same
    //        \key hourly
    //        \key daily
    //        \key monthly
    //        \key runperiod
    //        \key environment
    //        \key annual
    //        \note runperiod, environment, and annual are synonymous
    // Report MeterFileOnly,
    //        \memo same reporting as Report Meter -- goes to eplusout.mtr only
    //   A1 , \field Meter_Name
    //        \required-field
    //        \note Form is EnergyUseType:..., e.g. Electricity:* for all Electricity meters
    //        \note or EndUse:..., e.g. InteriorLights:* for all interior lights
    //        \note Report MeterFileOnly puts results on the eplusout.mtr file only
    //   A2 ; \field Reporting_Frequency
    //        \type choice
    //        \key timestep
    //        \note timestep refers to the zone timestep/timestep in hour value
    //        \note runperiod, environment, and annual are the same
    //        \key hourly
    //        \key daily
    //        \key monthly
    //        \key runperiod
    //        \key environment
    //        \key annual
    //        \note runperiod, environment, and annual are synonymous

    // Using/Aliasing
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    Array1D_string Alphas(2);
    Array1D<Real64> Numbers(1);
    int NumAlpha;
    int NumNumbers;
    int IOStat;
    int NumReqMeters;
    int NumReqMeterFOs;

    bool ErrorsFound(false); // If errors detected in input
    auto &op = state.dataOutputProcessor;
    auto &ipsc = state.dataIPShortCut;

    GetCustomMeterInput(state, ErrorsFound);
    if (ErrorsFound) {
        op->ErrorsLogged = true;
    }

    // Helper lambda to locate a meter index from its name. Returns a negative value if not found
    auto setupMeterFromMeterName = // (AUTO_OK_LAMBDA)
        [&state](std::string &name, ReportFreq freq, bool MeterFileOnlyIndicator, bool CumulativeIndicator) -> bool {
        bool result = false;

        size_t varnameLen = index(name, '[');
        if (varnameLen != std::string::npos) {
            name.erase(varnameLen);
        }

        auto &op = state.dataOutputProcessor;

        std::string::size_type wildCardPosition = index(name, '*');

        if (wildCardPosition == std::string::npos) {
            if (auto found = op->meterMap.find(name); found != op->meterMap.end()) {
                SetInitialMeterReportingAndOutputNames(state, found->second, MeterFileOnlyIndicator, freq, CumulativeIndicator);
                result = true;
            }
        } else { // Wildcard input
            std::string nameSubstr = name.substr(0, wildCardPosition);
            for (int iMeter = 0; iMeter < (int)op->meters.size(); ++iMeter) {
                if (UtilityRoutines::SameString(op->meters[iMeter]->Name.substr(0, wildCardPosition), nameSubstr)) {
                    SetInitialMeterReportingAndOutputNames(state, iMeter, MeterFileOnlyIndicator, freq, CumulativeIndicator);
                    result = true;
                }
            }
        }

        return result;
    };

    ipsc->cCurrentModuleObject = "Output:Meter";
    NumReqMeters = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

    for (Loop = 1; Loop <= NumReqMeters; ++Loop) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 ipsc->cCurrentModuleObject,
                                                                 Loop,
                                                                 Alphas,
                                                                 NumAlpha,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 IOStat,
                                                                 ipsc->lNumericFieldBlanks,
                                                                 ipsc->lAlphaFieldBlanks,
                                                                 ipsc->cAlphaFieldNames,
                                                                 ipsc->cNumericFieldNames);

        bool meterFileOnlyIndicator = false;
        bool cumulativeIndicator = false;
        ReportFreq freq = determineFrequency(state, UtilityRoutines::MakeUPPERCase(Alphas(2)));
        
        if (!setupMeterFromMeterName(Alphas(1), freq, meterFileOnlyIndicator, cumulativeIndicator)) {
            ShowWarningError(
                state, format("{}: invalid {}=\"{}\" - not found.", ipsc->cCurrentModuleObject, ipsc->cAlphaFieldNames(1), Alphas(1)));
        }
    }

    ipsc->cCurrentModuleObject = "Output:Meter:MeterFileOnly";
    NumReqMeterFOs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
    for (Loop = 1; Loop <= NumReqMeterFOs; ++Loop) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 ipsc->cCurrentModuleObject,
                                                                 Loop,
                                                                 Alphas,
                                                                 NumAlpha,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 IOStat,
                                                                 ipsc->lNumericFieldBlanks,
                                                                 ipsc->lAlphaFieldBlanks,
                                                                 ipsc->cAlphaFieldNames,
                                                                 ipsc->cNumericFieldNames);

        bool meterFileOnlyIndicator = true;
        bool cumulativeIndicator = false;
        ReportFreq freq = determineFrequency(state, UtilityRoutines::MakeUPPERCase(Alphas(2)));
        if (!setupMeterFromMeterName(Alphas(1), freq, meterFileOnlyIndicator, cumulativeIndicator)) {
            ShowWarningError(
                state, format("{}: invalid {}=\"{}\" - not found.", ipsc->cCurrentModuleObject, ipsc->cAlphaFieldNames(1), Alphas(1)));
        }
    }

    ipsc->cCurrentModuleObject = "Output:Meter:Cumulative";
    NumReqMeters = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

    for (Loop = 1; Loop <= NumReqMeters; ++Loop) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 ipsc->cCurrentModuleObject,
                                                                 Loop,
                                                                 Alphas,
                                                                 NumAlpha,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 IOStat,
                                                                 ipsc->lNumericFieldBlanks,
                                                                 ipsc->lAlphaFieldBlanks,
                                                                 ipsc->cAlphaFieldNames,
                                                                 ipsc->cNumericFieldNames);

        bool meterFileOnlyIndicator = false;
        bool cumulativeIndicator = true;
        ReportFreq freq = determineFrequency(state, UtilityRoutines::MakeUPPERCase(Alphas(2)));
        if (!setupMeterFromMeterName(Alphas(1), freq, meterFileOnlyIndicator, cumulativeIndicator)) {
            ShowWarningError(
                state, format("{}: invalid {}=\"{}\" - not found.", ipsc->cCurrentModuleObject, ipsc->cAlphaFieldNames(1), Alphas(1)));
        }
    }

    ipsc->cCurrentModuleObject = "Output:Meter:Cumulative:MeterFileOnly";
    NumReqMeterFOs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
    for (Loop = 1; Loop <= NumReqMeterFOs; ++Loop) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 ipsc->cCurrentModuleObject,
                                                                 Loop,
                                                                 Alphas,
                                                                 NumAlpha,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 IOStat,
                                                                 ipsc->lNumericFieldBlanks,
                                                                 ipsc->lAlphaFieldBlanks,
                                                                 ipsc->cAlphaFieldNames,
                                                                 ipsc->cNumericFieldNames);

        bool meterFileOnlyIndicator = true;
        bool cumulativeIndicator = true;
        ReportFreq freq = determineFrequency(state, UtilityRoutines::MakeUPPERCase(Alphas(2)));
        if (!setupMeterFromMeterName(Alphas(1), freq, meterFileOnlyIndicator, cumulativeIndicator)) {
            ShowWarningError(
                state, format("{}: invalid {}=\"{}\" - not found.", ipsc->cCurrentModuleObject, ipsc->cAlphaFieldNames(1), Alphas(1)));
        }
    }

    ReportMeterDetails(state);

    if (op->ErrorsLogged) {
        ShowFatalError(state, "UpdateMeterReporting: Previous Meter Specification errors cause program termination.");
    }

    op->meterValues.reserve(op->meters.size());
    std::fill(op->meterValues.begin(), op->meterValues.end(), 0.0);
}

void SetInitialMeterReportingAndOutputNames(EnergyPlusData &state,
                                            int const WhichMeter,              // Which meter number
                                            bool const MeterFileOnlyIndicator, // true if this is a meter file only reporting
                                            OutputProcessor::ReportFreq freq, // at what frequency is the meter reported
                                            bool const CumulativeIndicator // true if this is a Cumulative meter reporting
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2007

    // PURPOSE OF THIS SUBROUTINE:
    // Set values and output initial names to output files.

    // Using/Aliasing
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string indexGroup;
    auto &op = state.dataOutputProcessor;

    auto *meter = op->meters[WhichMeter];
    
    if (freq == ReportFreq::EachCall)
        freq = ReportFreq::TimeStep;
    
    auto &period = meter->periods[(int)freq];
    if (!CumulativeIndicator) {
        if (MeterFileOnlyIndicator && period.Rpt) {
            ShowWarningError(state,
                             format("Output:Meter:MeterFileOnly requested for \"{}\" ({}), already on \"Output:Meter\". Will report to both {} and {}",
                                    meter->Name, reportFreqNames[(int)freq],
                                    state.files.eso.filePath.filename().string(), state.files.mtr.filePath.filename().string()));
        }
        if (!period.Rpt) {
            period.Rpt = true;
            if (MeterFileOnlyIndicator) period.RptFO = true;
            int indexGroupKey = DetermineIndexGroupKeyFromMeterName(state, meter->Name);
            std::string indexGroup = DetermineIndexGroupFromMeterGroup(meter);
            WriteMeterDictionaryItem(state,
                                     freq, 
                                     StoreType::Summed,
                                     period.RptNum,
                                     indexGroupKey,
                                     indexGroup,
                                     meter->Name,
                                     meter->units,
                                     false,
                                     MeterFileOnlyIndicator);
        }
    } else { // !CumulativeIndicator
        if (MeterFileOnlyIndicator && period.accRpt) {
            ShowWarningError(state,
                             format("Output:Meter:MeterFileOnly requested for \"Cumulative {}\" (TimeStep), already on \"Output:Meter\". "
                                    "Will report to both {} and {}",
                                    meter->Name, reportFreqNames[(int)freq],
                                    state.files.eso.filePath.filename().string(), state.files.mtr.filePath.filename().string()));
        }

        if (!period.accRpt) {
            period.accRpt = true;
            if (MeterFileOnlyIndicator) period.accRptFO = true;
            int indexGroupKey = DetermineIndexGroupKeyFromMeterName(state, meter->Name);
            std::string indexGroup = DetermineIndexGroupFromMeterGroup(op->meters[WhichMeter]);
            WriteMeterDictionaryItem(state,
                                     freq,
                                     StoreType::Summed,
                                     period.accRptNum,
                                     indexGroupKey,
                                     indexGroup,
                                     meter->Name,
                                     meter->units,
                                     true,
                                     MeterFileOnlyIndicator);
        }
    } // if (CumulativeIndicator)
}

int GetMeterIndex(EnergyPlusData &state, std::string const &name)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2002

    // PURPOSE OF THIS FUNCTION:
    // This function returns a index to the meter "number" (aka assigned report number)
    // for the meter name.  If none active for this run, a zero is returned.  This is used later to
    // obtain a meter "value".

    auto &op = state.dataOutputProcessor;

    auto found = op->meterMap.find(name);
    return (found != op->meterMap.end()) ? found->second : -1;
}

Constant::eResource GetMeterResourceType(EnergyPlusData &state, int const MeterNumber) // Which Meter Number (from GetMeterIndex)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2002

    // Using/Aliasing
    using namespace OutputProcessor;

    return (MeterNumber != -1) ? state.dataOutputProcessor->meters[MeterNumber]->resource : Constant::eResource::Invalid;
}

Real64 GetCurrentMeterValue(EnergyPlusData &state, int const MeterNumber) // Which Meter Number (from GetMeterIndex)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2002

    // PURPOSE OF THIS FUNCTION:
    // This function returns the current meter value (timestep) for the meter number indicated.

    return (MeterNumber != -1) ? state.dataOutputProcessor->meters[MeterNumber]->CurTSValue : 0.0;
}

Real64 GetInstantMeterValue(EnergyPlusData &state,
                            int const meterNum,                             // Which Meter Number (from GetMeterIndex)
                            OutputProcessor::TimeStepType const timeStepType // Whether this is zone of HVAC
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   February 2003

    // PURPOSE OF THIS FUNCTION:
    // This function returns the Instantaneous meter value (timestep) for the meter number indicated
    //  using TimeStepType to differentiate between Zone and HVAC values.

    // Using/Aliasing
    using namespace OutputProcessor;

    Real64 InstantMeterValue = 0.0;

    if (meterNum == -1) return InstantMeterValue;

    auto &op = state.dataOutputProcessor;
    auto *meter = op->meters[meterNum];

    if (meter->type == MeterType::Normal) {
        for (int srcVarNum : meter->srcVarNums) { 
            auto *var = op->outVars[srcVarNum];
            // Separate the Zone variables from the HVAC variables using TimeStepType
            if (var->timeStepType != timeStepType) continue;
            
            auto *rVar = dynamic_cast<OutVarReal *>(var);
            assert(rVar != nullptr);
            // Add to the total all of the appropriate variables
            InstantMeterValue += (*rVar->Which) * rVar->ZoneMult * rVar->ZoneListMult;
        }
        
    } else if (meter->type == MeterType::CustomDec) { 
        auto *decMeter = op->meters[meter->decMeterNum];
        int decVarNum = decMeter->outVarNum;
        auto *decVar = op->outVars[decVarNum];

        if (decVar->timeStepType != timeStepType) {
            auto *decRVar = dynamic_cast<OutVarReal *>(decVar);
            assert(decRVar != nullptr);
            InstantMeterValue += (*decRVar->Which) * decRVar->ZoneMult * decRVar->ZoneListMult;
        }
        for (int srcVarNum : meter->srcVarNums) {
            auto *var = op->outVars[srcVarNum];
            
            if (var->timeStepType != timeStepType) continue;
            
            auto *rVar = dynamic_cast<OutVarReal *>(var);
            assert(rVar != nullptr);
            InstantMeterValue -= (*rVar->Which) * rVar->ZoneMult * rVar->ZoneListMult;
        }
    } else {
        assert(false);
    }
    
    return InstantMeterValue;
}

Real64 GetInternalVariableValue(EnergyPlusData &state,
                                OutputProcessor::VariableType const varType, // 1=integer, 2=real, 3=meter
                                int const keyVarIndex                        // Array index
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 2000
    //       MODIFIED       August 2003, M. J. Witte

    // PURPOSE OF THIS FUNCTION:
    // This function returns the current value of the Internal Variable assigned to
    // the varType and keyVarIndex.  Values may be accessed for REAL(r64) and integer
    // report variables and meter variables.  The variable type (varType) may be
    // determined by calling subroutine and GetVariableKeyCountandType.  The
    // index (keyVarIndex) may be determined by calling subroutine GetVariableKeys.

    // METHODOLOGY EMPLOYED:
    // Uses Internal OutputProcessor data structure to return value.

    // Using/Aliasing
    using namespace OutputProcessor;
    using ScheduleManager::GetCurrentScheduleValue;

    // Return value
    Real64 resultVal; // value returned

    auto &op = state.dataOutputProcessor;

    // Select based on variable type:  integer, real, or meter
    if (varType == VariableType::Invalid) { // Variable not a found variable
        resultVal = 0.0;
    } else if (varType == VariableType::Integer || varType == VariableType::Real) {
        if (keyVarIndex < 0 || keyVarIndex >= (int)op->outVars.size()) {
            ShowFatalError(state, "GetInternalVariableValue: passed variable index beyond range of array.");
            ShowContinueError(state, format("Index = {} Number of variables = {}", keyVarIndex, op->outVars.size()));
        }

        // must use %Which, %Value is always zero if variable is not a requested report variable
        resultVal = (varType == VariableType::Integer) ? 
                (double)*(dynamic_cast<OutVarInt *>(op->outVars[keyVarIndex]))->Which : 
                (double)*(dynamic_cast<OutVarReal *>(op->outVars[keyVarIndex]))->Which;
    } else if (varType == VariableType::Meter) {
        resultVal = GetCurrentMeterValue(state, keyVarIndex);
    } else if (varType == VariableType::Schedule) {
        resultVal = GetCurrentScheduleValue(state, keyVarIndex);
    } else {
        resultVal = 0.0;
    }

    return resultVal;
}

Real64 GetInternalVariableValueExternalInterface(EnergyPlusData &state,
                                                 OutputProcessor::VariableType const varType, // 1=integer, 2=REAL(r64), 3=meter
                                                 int const keyVarIndex                        // Array index
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Thierry S. Nouidui
    //       DATE WRITTEN   August 2011

    // PURPOSE OF THIS FUNCTION:
    // This function returns the last zone-timestep value of the Internal Variable assigned to
    // the varType and keyVarIndex.  Values may be accessed for REAL(r64) and integer
    // report variables and meter variables.  The variable type (varType) may be
    // determined by calling subroutine and GetVariableKeyCountandType.  The
    // index (keyVarIndex) may be determined by calling subroutine GetVariableKeys.

    // METHODOLOGY EMPLOYED:
    // Uses Internal OutputProcessor data structure to return value.

    // Using/Aliasing
    using namespace OutputProcessor;
    using ScheduleManager::GetCurrentScheduleValue;

    // Return value
    Real64 resultVal; // value returned

    auto &op = state.dataOutputProcessor;

    // Select based on variable type:  integer, REAL(r64), or meter
    if (varType == VariableType::Invalid) { // Variable not a found variable
        resultVal = 0.0;
    } else if (varType == VariableType::Integer || varType == VariableType::Real) {
        if (keyVarIndex < 0 || keyVarIndex >= (int)op->outVars.size()) {
            ShowFatalError(state, "GetInternalVariableValueExternalInterface: passed index beyond range of array.");
        } 

        resultVal = (double)op->outVars[keyVarIndex]->EITSValue;
    } else if (varType == VariableType::Meter) {
        resultVal = GetCurrentMeterValue(state, keyVarIndex);
    } else if (varType == VariableType::Schedule) {
        resultVal = GetCurrentScheduleValue(state, keyVarIndex);
    } else {
        resultVal = 0.0;
    }

    return resultVal;
}

int GetNumMeteredVariables(EnergyPlusData &state,
                           [[maybe_unused]] std::string const &ComponentType, // Given Component Type
                           std::string const &ComponentName                   // Given Component Name (user defined)
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005

    // PURPOSE OF THIS FUNCTION:
    // This function counts the number of metered variables associated with the
    // given ComponentType/Name.   This resultant number would then be used to
    // allocate arrays for a call the GetMeteredVariables routine.

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int NumVariables = 0;
    auto &op = state.dataOutputProcessor;

    for (auto *var : op->outVars) {
        //    Pos=INDEX(RVariableTypes(Loop)%VarName,':')
        //    IF (ComponentName /= RVariableTypes(Loop)%VarNameUC(1:Pos-1)) CYCLE
        if (var->varType != OutputProcessor::VariableType::Real) continue;
        if (ComponentName != var->keyUC) continue;
        if (var->meterNums.size() > 0) { 
            ++NumVariables;
        }
    }
    return NumVariables;
}

int GetMeteredVariables(EnergyPlusData &state,
                        std::string const &ComponentName,                      // Given Component Name (user defined)
                        Array1D<OutputProcessor::MeteredVar> &meteredVars     // Variable Types (1=integer, 2=real, 3=meter)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005
    //       MODIFIED       Jason DeGraw 2/12/2020, de-optionalized
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine gets the variable names and other associated information
    // for metered variables associated with the given ComponentType/Name.

    // Using/Aliasing
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumVariables;
    auto &op = state.dataOutputProcessor;

    NumVariables = 0;

    for (int iVar = 0; iVar < (int)op->outVars.size(); ++iVar) {
        //    Pos=INDEX(RVariableTypes(Loop)%VarName,':')
        //    IF (ComponentName /= RVariableTypes(Loop)%VarNameUC(1:Pos-1)) CYCLE
        auto *var = op->outVars[iVar];
        if (var->varType != VariableType::Real) continue;
        if (ComponentName != var->keyUC) continue;
        if (var->meterNums.size() == 0) continue;

        auto &meteredVar = meteredVars(++NumVariables);

        meteredVar.num = iVar;
        meteredVar.varType = VariableType::Real;
        meteredVar.timeStepType = var->timeStepType;
        meteredVar.units = var->units;
        
        meteredVar.resource = op->meters[var->meterNums[0]]->resource;
        meteredVar.name = var->keyColonNameUC;

        bool foundEndUse = false;
        bool foundGroup = false;
        for (int meterNum : var->meterNums) {
            auto *meter = op->meters[meterNum];
            if (!foundEndUse && !meter->EndUse.empty()) {
                meteredVar.endUse = UtilityRoutines::MakeUPPERCase(meter->EndUse);
                foundEndUse = true;
            } 

            if (!foundGroup && !meter->Group.empty()) {
                meteredVar.group = UtilityRoutines::MakeUPPERCase(meter->Group);
                foundGroup = true;
            }

            if (foundEndUse && foundGroup) break;
        }
        
        meteredVar.rptNum = var->ReportID;
    }
    return NumVariables;
}

void GetVariableKeyCountandType(EnergyPlusData &state,
                                std::string const &name, // Standard variable name
                                int &numKeys,               // Number of keys found
                                OutputProcessor::VariableType &varType,
                                OutputProcessor::StoreType &storeType,      // Variable  is Averaged=1 or Summed=2
                                OutputProcessor::TimeStepType &timeStepType, // Variable time step is Zone=1 or HVAC=2
                                Constant::Units &units             // Units enumeration
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael J. Witte
    //       DATE WRITTEN   August 2003

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine returns the variable TYPE (Real, integer, meter, schedule, etc.)
    // (varType) whether it is an averaged or summed variable (varAvgSum),
    // whether it is a zone or HVAC time step (varStepType),
    // and the number of keynames for a given report variable or report meter name
    // (varName).  The variable type (varType) and number of keys (numKeys) are
    // used when calling subroutine GetVariableKeys to obtain a list of the
    // keynames for a particular variable and a corresponding list of indexes.

    // METHODOLOGY EMPLOYED:
    // Uses Internal OutputProcessor data structure to search for varName
    // in each of the three output data arrays:
    //       RVariableTypes - real report variables
    //       IVariableTypes - integer report variables
    //       EnergyMeters   - report meters (via GetMeterIndex function)
    //       Schedules      - specific schedule values
    // When the variable is found, the variable type (varType) is set and the
    // number of associated keys is counted.

    // Using/Aliasing
    using namespace OutputProcessor;
    using ScheduleManager::GetScheduleIndex;
    using ScheduleManager::GetScheduleType;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    auto &op = state.dataOutputProcessor;

    varType = VariableType::Invalid;
    numKeys = 0;
    storeType = StoreType::Averaged;
    timeStepType = TimeStepType::Zone;
    units = Constant::Units::Invalid;

    std::string nameUC = UtilityRoutines::MakeUPPERCase(name);

    // Search Variable List First
    if (auto found = op->ddOutVarMap.find(nameUC); found != op->ddOutVarMap.end()) {
        auto const *ddOutVar = op->ddOutVars[found->second];
        varType = ddOutVar->variableType;
        storeType = ddOutVar->storeType;
        timeStepType = ddOutVar->timeStepType;
        units = ddOutVar->units;
        numKeys = ddOutVar->keyOutVarNums.size();

    } else if (auto found = op->meterMap.find(nameUC); found != op->meterMap.end()) { 
        // Search Meters if not found in integers or reals
        // Use the GetMeterIndex function
        // Meters do not have keys, so only one will be found
        int meterNum = found->second;
        numKeys = 1;
        varType = VariableType::Meter;
        units = op->meters[meterNum]->units;
        storeType = StoreType::Summed;
        timeStepType = TimeStepType::Zone;

    } else {
        // Search schedules if not found in integers, reals, or meters
        // Use the GetScheduleIndex function
        // Schedules do not have keys, so only one will be found
        int schedNum = GetScheduleIndex(state, nameUC);
        if (schedNum > 0) {
            numKeys = 1;
            varType = VariableType::Schedule;
            units = static_cast<Constant::Units>(getEnumerationValue(Constant::unitNamesUC, UtilityRoutines::MakeUPPERCase(GetScheduleType(state, schedNum))));
            storeType = StoreType::Averaged;
            timeStepType = TimeStepType::Zone;
        }
    }
}

void GetVariableKeys(EnergyPlusData &state,
                     std::string const &varName,                  // Standard variable name
                     OutputProcessor::VariableType const varType, // 1=integer, 2=real, 3=meter
                     Array1D_int &keyOutVarNums                   // Array index for
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael J. Witte
    //       DATE WRITTEN   August 2003

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine returns a list of keynames and indexes associated
    // with a particular report variable or report meter name (varName).
    // This routine assumes that the variable TYPE (Real, integer, meter, etc.)
    // may be determined by calling GetVariableKeyCountandType.  The variable type
    // and index can then be used with function GetInternalVariableValue to
    // to retrieve the current value of a particular variable/keyname combination.

    // METHODOLOGY EMPLOYED:
    // Uses Internal OutputProcessor data structure to search for varName
    // and build list of keynames and indexes.  The indexes are the array index
    // in the data array for the

    // Using/Aliasing
    using namespace OutputProcessor;
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string nameUC = UtilityRoutines::MakeUPPERCase(varName);

    auto &op = state.dataOutputProcessor;

    // Select based on variable type:  integer, real, or meter
    if (varType == VariableType::Integer || varType == VariableType::Real) { 
        auto found = op->ddOutVarMap.find(nameUC);
        if (found == op->ddOutVarMap.end())
            return;
        
        auto const *ddOutVar = op->ddOutVars[found->second];

        if (ddOutVar->keyOutVarNums.size() > size(keyOutVarNums)) {
            ShowFatalError(state, "Invalid array size in GetVariableKeys");
        }

        int iKey = 0;
        for (int keyOutVarNum : ddOutVar->keyOutVarNums) {
            ++iKey;
            keyOutVarNums(iKey) = keyOutVarNum;
        }
        
    } else if (varType == VariableType::Meter) { // Meter

        if (size(keyOutVarNums) == 0) {
            ShowFatalError(state, "Invalid array size in GetVariableKeys");
        }
        keyOutVarNums(1) = GetMeterIndex(state, varName);

    } else if (varType == VariableType::Schedule) { // Schedule

        if (size(keyOutVarNums) == 0) {
            ShowFatalError(state, "Invalid array size in GetVariableKeys");
        }
        keyOutVarNums(1) = GetScheduleIndex(state, varName);
    } else {
        // do nothing
    }
}

bool ReportingThisVariable(EnergyPlusData &state, std::string const &RepVarName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2008

    // PURPOSE OF THIS FUNCTION:
    // This function scans the report variables and reports back
    // if user has requested this variable be reported.
    using namespace OutputProcessor;
        
    auto &op = state.dataOutputProcessor;

    for (int iReqVar = 0; iReqVar < (int)op->reqVars.size(); ++iReqVar) {
        if (op->reqVars[iReqVar]->name == RepVarName) return true;
    }

    if (auto found = op->meterMap.find(RepVarName); found != op->meterMap.end()) {
        auto const *meter = op->meters[found->second];
        for (int iFreq = (int)ReportFreq::TimeStep; iFreq < (int)ReportFreq::Num; ++iFreq) {
            if (iFreq == (int)ReportFreq::Year) continue;
            auto const &period = meter->periods[iFreq];
            if (period.Rpt || period.RptFO || period.accRpt || period.accRptFO)
                return true;
        }
    }

    return false;
}

void InitPollutionMeterReporting(EnergyPlusData &state, OutputProcessor::ReportFreq freq)
{

    // SUBROUTINE INFORMATION:Richard Liesen
    //       DATE WRITTEN   July 2002

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is called at the end of the first HVAC iteration and
    // sets up the reporting for the Pollution Meters.
    // ReportPollutionOutput,
    //   A1 ; \field Reporting_Frequency
    //        \type choice
    //        \key timestep
    //        \key hourly
    //        \key daily
    //        \key monthly
    //        \key runperiod
    // METHODOLOGY EMPLOYED:
    // The program tries to setup all of the following meters if the Pollution Report is initiated.
    //       Electricity:Facility [J]
    //       Diesel:Facility [J]
    //       DistrictCooling:Facility [J]
    //       DistrictHeating:Facility [J]
    //       Gas:Facility [J]
    //       GASOLINE:Facility [J]
    //       COAL:Facility [J]
    //       FuelOilNo1:Facility [J]
    //       FuelOilNo2:Facility [J]
    //       Propane:Facility [J]
    //       ElectricityProduced:Facility [J]
    //       Pollutant:CO2
    //       Pollutant:CO
    //       Pollutant:CH4
    //       Pollutant:NOx
    //       Pollutant:N2O
    //       Pollutant:SO2
    //       Pollutant:PM
    //       Pollutant:PM10
    //       Pollutant:PM2.5
    //       Pollutant:NH3
    //       Pollutant:NMVOC
    //       Pollutant:Hg
    //       Pollutant:Pb
    //       Pollutant:WaterEnvironmentalFactors
    //       Pollutant:Nuclear High
    //       Pollutant:Nuclear Low
    //       Pollutant:Carbon Equivalent

    // Using/Aliasing
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    auto &op = state.dataOutputProcessor;

    for (int iResource = 0; iResource < (int)Constant::eResource::Num; ++iResource) {
        std::string meterName = format("{}:Facility", Constant::eResourceNames[iResource]);
        std::string meterNameUC = UtilityRoutines::MakeUPPERCase(meterName);

        auto found = op->meterMap.find(meterNameUC);
        if (found == op->meterMap.end()) continue;

        auto *meter = op->meters[found->second];
        auto &period = meter->periods[(int)freq];
        
        int indexGroupKey = DetermineIndexGroupKeyFromMeterName(state, meter->Name);
        std::string indexGroup = DetermineIndexGroupFromMeterGroup(meter);

        // All of the specified meters are checked and the headers printed to the meter file if this
        //  has not been done previously
        if (!period.Rpt) { 
            period.Rpt = true;
            WriteMeterDictionaryItem(state, freq, StoreType::Summed, period.RptNum, indexGroupKey, indexGroup, meter->Name, meter->units, false, false);
            op->freqTrackingVariables[(int)freq] = true;
        }
    }
}

void ProduceRDDMDD(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2009

    // PURPOSE OF THIS SUBROUTINE:
    // provide a single call for writing out the Report Data Dictionary and Meter Data Dictionary.

    // Using/Aliasing
    using namespace OutputProcessor;
    using General::ScanForReports;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string VarOption1;
    std::string VarOption2;
    bool DoReport;
    bool SortByName;
    int ItemPtr;

    struct VariableTypes
    {
        // Members
        int RealIntegerType = 0; // Real= 1, Integer=2
        int VarPtr = 0;          // pointer to real/integer VariableTypes structures
        int TimeStepType = 0;
        int StoreType = 0;
        std::string UnitsString;
    };

    auto &op = state.dataOutputProcessor;
    auto &rf = state.dataResultsFramework->resultsFramework;

    //  See if Report Variables should be turned on
    SortByName = false;
    ScanForReports(state, "VariableDictionary", DoReport, _, VarOption1, VarOption2);
    //  IF (.not. DoReport) RETURN

    if (DoReport) {
        op->ProduceReportVDD = ReportVDD::Yes;
        if (VarOption1 == std::string("IDF")) {
            op->ProduceReportVDD = ReportVDD::IDF;
        }
        if (!VarOption2.empty()) {
            if (UtilityRoutines::SameString(VarOption2, "Name") || UtilityRoutines::SameString(VarOption2, "AscendingName")) {
                SortByName = true;
            }
        }
    }

    state.files.rdd.ensure_open(state, "ProduceRDDMDD", state.files.outputControl.rdd);
    state.files.mdd.ensure_open(state, "ProduceRDDMDD", state.files.outputControl.mdd);
    if (op->ProduceReportVDD == ReportVDD::Yes) {
        print(state.files.rdd, "Program Version,{},{}{}", state.dataStrGlobals->VerStringVar, state.dataStrGlobals->IDDVerString, '\n');
        print(state.files.rdd, "Var Type (reported time step),Var Report Type,Variable Name [Units]{}", '\n');

        print(state.files.mdd, "Program Version,{},{}{}", state.dataStrGlobals->VerStringVar, state.dataStrGlobals->IDDVerString, '\n');
        print(state.files.mdd, "Var Type (reported time step),Var Report Type,Variable Name [Units]{}", '\n');
    } else if (op->ProduceReportVDD == ReportVDD::IDF) {
        print(state.files.rdd, "! Program Version,{},{}{}", state.dataStrGlobals->VerStringVar, state.dataStrGlobals->IDDVerString, '\n');
        print(state.files.rdd, "! Output:Variable Objects (applicable to this run){}", '\n');

        print(state.files.mdd, "! Program Version,{},{}{}", state.dataStrGlobals->VerStringVar, state.dataStrGlobals->IDDVerString, '\n');
        print(state.files.mdd, "! Output:Meter Objects (applicable to this run){}", '\n');
    }

    if (op->ProduceReportVDD == ReportVDD::Yes || op->ProduceReportVDD == ReportVDD::IDF) {
        auto miVar = op->ddOutVarMap.begin();
        for (int aiVar = 0; aiVar < (int)op->outVars.size() && miVar != op->ddOutVarMap.end(); ++aiVar, ++miVar) {
            int iVar = (SortByName) ? miVar->second : aiVar; // Choose either map (sorted) order or array (unsorted) order
            auto *ddVar = op->ddOutVars[iVar];

            if (ddVar->ReportedOnDDFile) continue;

            std::string_view timeStepName = timeStepNames[(int)ddVar->timeStepType];
            std::string_view storeTypeName = storeTypeNames[(int)ddVar->storeType];
            std::string_view varName = ddVar->name;
            std::string_view unitName = (ddVar->units == Constant::Units::customEMS) ? ddVar->unitNameCustomEMS : Constant::unitNames[(int)ddVar->units];
            if (op->ProduceReportVDD == ReportVDD::Yes) {
                print(state.files.rdd, "{},{},{} [{}]\n", timeStepName, storeTypeName, varName, unitName);
                rf->RDD.push_back(format("{},{},{} [{}]", timeStepName, storeTypeName, varName, unitName));
            } else {
                print(state.files.rdd, "Output:Variable,*,{},hourly; !- {} {} [{}]\n", varName, timeStepName, storeTypeName, unitName);
                rf->RDD.push_back(format("{},{},{} [{}]", timeStepName, storeTypeName, varName, unitName));
            }
                
            ddVar->ReportedOnDDFile = true;
            while (ddVar->Next != 0) {
                if (SortByName) {
                    ++ItemPtr;
                } else {
                    ItemPtr = ddVar->Next;
                }
                ddVar = op->ddOutVars[ItemPtr];

                std::string_view timeStepName = timeStepNames[(int)ddVar->timeStepType];
                std::string_view storeTypeName = storeTypeNames[(int)ddVar->storeType];
                std::string_view varName = ddVar->name;
                std::string_view unitName = (ddVar->units == Constant::Units::customEMS) ? ddVar->unitNameCustomEMS : Constant::unitNames[(int)ddVar->units];
                
                if (op->ProduceReportVDD == ReportVDD::Yes) {
                    print(state.files.rdd, "{},{},{} [{}]\n", timeStepName, storeTypeName, varName, unitName);
                    rf->RDD.push_back(format("{},{},{} [{}]", timeStepName, storeTypeName, varName, unitName));
                } else {
                    print(state.files.rdd, "Output:Variable,*,{},hourly; !- {} {} [{}]\n", varName, timeStepName, storeTypeName, unitName);
                    rf->RDD.push_back(format("{},{},{} [{}]", timeStepName, storeTypeName, varName, unitName));
                }
                ddVar->ReportedOnDDFile = true;
            } // while (ddVar->Next != 0)
        } // for (aiVar, miVar) 
    } // if (produceReportVDD)
    state.files.rdd.close();
    
    auto miMeter = op->meterMap.begin();
    for (int aiMeter = 0; aiMeter < (int)op->meters.size() && miMeter != op->meterMap.end(); ++aiMeter, ++miMeter) {
        int iMeter = (SortByName) ? miMeter->second : aiMeter;
        auto *meter = op->meters[iMeter];
        std::string_view unitName = Constant::unitNames[(int)meter->units];
        if (op->ProduceReportVDD == ReportVDD::Yes) {
            print(state.files.mdd, "Zone,Meter,{}[{}]\n", meter->Name, unitName);
            rf->MDD.push_back(format("Zone,Meter,{},[{}]", meter->Name, unitName));
        } else if (op->ProduceReportVDD == ReportVDD::IDF) {
            print(state.files.mdd, "Output:Meter,{},hourly; !-[{}]\n", meter->Name, unitName);
            rf->MDD.push_back(format("Output:Meter,{},[{}]", meter->Name, unitName));
            print(state.files.mdd, "Output:Meter:Cumulative,{},hourly; !-[{}]\n", meter->Name, unitName);
            rf->MDD.push_back(format("Output:Meter:Cumulative,{},[{}]", meter->Name, unitName));
        }
    }
    state.files.mdd.close();
}

int AddDDOutVar(EnergyPlusData &state,
                std::string_view const name, // Variable Name
                OutputProcessor::TimeStepType const timeStepType,
                OutputProcessor::StoreType const storeType,
                OutputProcessor::VariableType const variableType,
                Constant::Units const units,
                std::string_view const customUnitName // the custom name for the units from EMS definition of units
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   August 2010

    // PURPOSE OF THIS SUBROUTINE:
    // This routine maintains a unique list of Output Variables for the
    // Variable Dictionary output.

    // Using/Aliasing
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    auto &op = state.dataOutputProcessor;

    std::string nameUC = std::string(name);
    
    auto found = op->ddOutVarMap.find(nameUC);
    if (found == op->ddOutVarMap.end()) {
        auto *ddVar = new DDOutVar();
        op->ddOutVars.push_back(ddVar);
        // Add to map
        op->ddOutVarMap.insert_or_assign(nameUC, op->ddOutVars.size() - 1);
        
        ddVar->timeStepType = timeStepType;
        ddVar->storeType = storeType;
        ddVar->variableType = variableType;
        ddVar->name = name;
        ddVar->units = units;
        if (!customUnitName.empty() && units == Constant::Units::customEMS) {
            ddVar->unitNameCustomEMS = customUnitName;
        }
        return op->ddOutVars.size() - 1;
        
    } else if (units == op->ddOutVars[found->second]->units) {
        return found->second;

    } else { // not the same as first units
        int dup2 = -1;                                           // for duplicate variable name
        auto *ddVarDup = op->ddOutVars[found->second];
        while (ddVarDup->Next != -1) {
            if (units != op->ddOutVars[ddVarDup->Next]->units) {
                ddVarDup = op->ddOutVars[ddVarDup->Next];
                continue;
            }
            dup2 = ddVarDup->Next;
            break;
        }
        if (dup2 == -1) {
            DDOutVar *ddVar2 = new DDOutVar();
            op->ddOutVars.push_back(ddVar2);
            // Don't add this one to the map.  Leave the map pointing to the first one
            ddVar2->timeStepType = timeStepType;
            ddVar2->storeType = storeType;
            ddVar2->variableType = variableType;
            ddVar2->name = name;
            ddVar2->units = units;
            if (!customUnitName.empty() && units == Constant::Units::customEMS) {
                ddVar2->unitNameCustomEMS = customUnitName;
            }
            ddVarDup->Next = op->ddOutVars.size() - 1;

            return op->ddOutVars.size() - 1;
        } else {
            return dup2;
        } // if (dup2 == 0)
    } // if (unitsForVar)
} // AddDDOutVar()

int initErrorFile(EnergyPlusData &state)
{
    state.files.err_stream = std::make_unique<std::ofstream>(state.files.outputErrFilePath);
    if (state.files.err_stream->bad()) {
        DisplayString(state, "ERROR: Could not open file " + state.files.outputErrFilePath.string() + " for output (write).");
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}

} // namespace EnergyPlus
