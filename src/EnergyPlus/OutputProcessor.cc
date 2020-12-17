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
#include <EnergyPlus/SortAndStringUtilities.hh>
#include <EnergyPlus/UtilityRoutines.hh>
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
    // Lots of pointers and other fancy data stuff.

    // Routines tagged on the end of this module:
    //  AddToOutputVariableList
    //  AssignReportNumber
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

    inline void ReallocateRVar(EnergyPlusData &state)
    {
        state.dataOutputProcessor->RVariableTypes.redimension(state.dataOutputProcessor->MaxRVariable += RVarAllocInc);
    }

    inline void ReallocateIVar(EnergyPlusData &state)
    {
        state.dataOutputProcessor->IVariableTypes.redimension(state.dataOutputProcessor->MaxIVariable += IVarAllocInc);
    }

    void InitializeOutput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes the OutputProcessor data structures.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        state.dataOutputProcessor->RVariableTypes.allocate(RVarAllocInc);
        state.dataOutputProcessor->MaxRVariable = RVarAllocInc;

        state.dataOutputProcessor->IVariableTypes.allocate(IVarAllocInc);
        state.dataOutputProcessor->MaxIVariable = IVarAllocInc;

        // First index is the frequency designation (-1 = each call, etc)
        // Second index is the variable type (1=Average, 2=Sum)
        // Note, Meters always report like Average (with min/max, etc) for hourly and above
        // FreqNotice( 1, -1 ) = " !Each Call";
        // FreqNotice( 1, 0 ) = " !TimeStep";
        // FreqNotice( 1, 1 ) = " !Hourly";
        // FreqNotice( 1, 2 ) = " !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]";
        // FreqNotice( 1, 3 ) = " !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]";
        // FreqNotice( 1, 4 ) = " !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]";
        // FreqNotice( 2, -1 ) = " !Each Call";
        // FreqNotice( 2, 0 ) = " !TimeStep";
        // FreqNotice( 2, 1 ) = " !Hourly";
        // FreqNotice( 2, 2 ) = " !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]";
        // FreqNotice( 2, 3 ) = " !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]";
        // FreqNotice( 2, 4 ) = " !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]";

        state.dataOutputProcessor->ReportList.allocate(500);
        state.dataOutputProcessor->NumReportList = 500;
        state.dataOutputProcessor->ReportList = 0;
        state.dataOutputProcessor->NumExtraVars = 0;

        // Initialize end use category names - the indices must match up with endUseNames in OutputReportTabular
        state.dataOutputProcessor->EndUseCategory.allocate(DataGlobalConstants::iEndUse.size());
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Heating)).Name = "Heating";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Cooling)).Name = "Cooling";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::InteriorLights)).Name = "InteriorLights";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)).Name = "ExteriorLights";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::InteriorEquipment)).Name = "InteriorEquipment";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::ExteriorEquipment)).Name = "ExteriorEquipment";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Fans)).Name = "Fans";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Pumps)).Name = "Pumps";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::HeatRejection)).Name = "HeatRejection";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Humidification)).Name = "Humidifier";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::HeatRecovery)).Name = "HeatRecovery";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::WaterSystem)).Name = "WaterSystems";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Refrigeration)).Name = "Refrigeration";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Cogeneration)).Name = "Cogeneration";

        // Initialize display names for output table - this could go away if end use key names are changed to match
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Heating)).DisplayName = "Heating";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Cooling)).DisplayName = "Cooling";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::InteriorLights)).DisplayName = "Interior Lighting";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::ExteriorLights)).DisplayName = "Exterior Lighting";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::InteriorEquipment)).DisplayName = "Interior Equipment";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::ExteriorEquipment)).DisplayName = "Exterior Equipment";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Fans)).DisplayName = "Fans";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Pumps)).DisplayName = "Pumps";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::HeatRejection)).DisplayName = "Heat Rejection";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Humidification)).DisplayName = "Humidification";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::HeatRecovery)).DisplayName = "Heat Recovery";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::WaterSystem)).DisplayName = "Water Systems";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Refrigeration)).DisplayName = "Refrigeration";
        state.dataOutputProcessor->EndUseCategory(DataGlobalConstants::iEndUse.at(DataGlobalConstants::EndUse::Cogeneration)).DisplayName = "Generators";

        state.dataOutputProcessor->OutputInitialized = true;

        state.dataOutputProcessor->TimeStepZoneSec = double(state.dataGlobal->MinutesPerTimeStep) * 60.0;

        InitializeMeters(state);
    }

    void SetupTimePointers(EnergyPlusData &state, std::string const &TimeStepTypeKey, // Which timestep is being set up, 'Zone'=1, 'HVAC'=2
                           Real64 &TimeStep                    // The timestep variable.  Used to get the address
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
        TimeStepType timeStepType = ValidateTimeStepType(state, TimeStepTypeKey, "SetupTimePointers");

        TimeSteps tPtr;
        tPtr.TimeStep = &TimeStep;
        if (!state.dataOutputProcessor->TimeValue.insert(std::make_pair(timeStepType, tPtr)).second) {
            // The element was already present... shouldn't happen
            ShowFatalError(state, "SetupTimePointers was already called for " + TimeStepTypeKey);
        }
    }

    void CheckReportVariable(EnergyPlusData &state, std::string const &KeyedValue, std::string const &VarName)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

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
        int Item;
        int Loop;
        int Pos;
        int MinLook;
        int MaxLook;

        // Make sure that input has been read
        GetReportVariableInput(state);

        if (state.dataOutputProcessor->NumOfReqVariables > 0) {
            // Do a quick check
            Item = UtilityRoutines::FindItem(VarName, state.dataOutputProcessor->ReqRepVars, &ReqReportVariables::VarName);

            state.dataOutputProcessor->NumExtraVars = 0;
            state.dataOutputProcessor->ReportList = 0;
            MinLook = 999999999;
            MaxLook = -999999999;

            if (Item != 0) {
                Loop = Item;
                Pos = Item;
                MinLook = min(MinLook, Pos);
                MaxLook = max(MaxLook, Pos);
                while (Loop <= state.dataOutputProcessor->NumOfReqVariables && Pos != 0) {
                    //  Mark all with blank keys as used
                    if (state.dataOutputProcessor->ReqRepVars(Loop).Key.empty()) {
                        state.dataOutputProcessor->ReqRepVars(Loop).Used = true;
                    }
                    if (Loop < state.dataOutputProcessor->NumOfReqVariables) {
                        Pos = UtilityRoutines::FindItem(VarName, state.dataOutputProcessor->ReqRepVars({Loop + 1, state.dataOutputProcessor->NumOfReqVariables}), &ReqReportVariables::VarName);
                        if (Pos != 0) {
                            MinLook = min(MinLook, Loop + Pos);
                            MaxLook = max(MaxLook, Loop + Pos);
                        }
                    } else {
                        Pos = 1;
                    }
                    Loop += Pos;
                }
                BuildKeyVarList(state, KeyedValue, VarName, MinLook, MaxLook);
                AddBlankKeys(state, VarName, MinLook, MaxLook);
            }
        }
    }

    void BuildKeyVarList(EnergyPlusData &state,
                         std::string const &KeyedValue,   // Associated Key for this variable
                         std::string const &VariableName, // String Name of variable
                         int const MinIndx,               // Min number (from previous routine) for this variable
                         int const MaxIndx                // Max number (from previous routine) for this variable
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   March 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine builds an initial list (from ReqRepVars) of
        // pointers to that data structure for this KeyedValue and VariableName.

        // METHODOLOGY EMPLOYED:
        // Go through the ReqRepVars list and add those
        // that match (and dont duplicate ones already in the list).

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int Loop1;
        bool Dup;

        for (Loop = MinIndx; Loop <= MaxIndx; ++Loop) {
            if (state.dataOutputProcessor->ReqRepVars(Loop).Key.empty()) continue;
            if (!UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(Loop).VarName, VariableName)) continue;
            if (!(UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(Loop).Key, KeyedValue) || RE2::FullMatch(KeyedValue, "(?i)" + state.dataOutputProcessor->ReqRepVars(Loop).Key)))
                continue;

            //   A match.  Make sure doesn't duplicate

            state.dataOutputProcessor->ReqRepVars(Loop).Used = true;
            Dup = false;
            for (Loop1 = 1; Loop1 <= state.dataOutputProcessor->NumExtraVars; ++Loop1) {
                if (state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop1)).frequency == state.dataOutputProcessor->ReqRepVars(Loop).frequency) {
                    Dup = true;
                } else {
                    continue;
                }
                //  So Same Report Frequency
                if (state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop1)).SchedPtr != state.dataOutputProcessor->ReqRepVars(Loop).SchedPtr) Dup = false;
            }

            if (!Dup) {
                ++state.dataOutputProcessor->NumExtraVars;
                if (state.dataOutputProcessor->NumExtraVars == state.dataOutputProcessor->NumReportList) {
                    state.dataOutputProcessor->ReportList.redimension(state.dataOutputProcessor->NumReportList += 100, 0);
                }
                state.dataOutputProcessor->ReportList(state.dataOutputProcessor->NumExtraVars) = Loop;
            }
        }
    }

    void AddBlankKeys(EnergyPlusData &state,
                      std::string const &VariableName, // String Name of variable
                      int const MinIndx,               // Min number (from previous routine) for this variable
                      int const MaxIndx                // Max number (from previous routine) for this variable
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   March 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine adds to the ReportList any report variables that have
        // been requested for all keys of that report variable (if it doesnt duplicate
        // a frequency already on the list).

        // METHODOLOGY EMPLOYED:
        // Go through the ReqRepVars list and add those
        // that match (and dont duplicate ones already in the list).

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int Loop1;
        bool Dup;

        for (Loop = MinIndx; Loop <= MaxIndx; ++Loop) {
            if (!state.dataOutputProcessor->ReqRepVars(Loop).Key.empty()) continue;
            if (!UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(Loop).VarName, VariableName)) continue;

            //   A match.  Make sure doesnt duplicate

            Dup = false;
            for (Loop1 = 1; Loop1 <= state.dataOutputProcessor->NumExtraVars; ++Loop1) {
                // IF (ReqRepVars(ReportList(Loop1))%ReportFreq == ReqRepVars(Loop)%ReportFreq) Dup=.TRUE.
                if (state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop1)).frequency == state.dataOutputProcessor->ReqRepVars(Loop).frequency) {
                    Dup = true;
                } else {
                    continue;
                }
                //  So Same Report Frequency
                if (state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop1)).SchedPtr != state.dataOutputProcessor->ReqRepVars(Loop).SchedPtr) Dup = false;
            }

            if (!Dup) {
                ++state.dataOutputProcessor->NumExtraVars;
                if (state.dataOutputProcessor->NumExtraVars == state.dataOutputProcessor->NumReportList) {
                    state.dataOutputProcessor->ReportList.redimension(state.dataOutputProcessor->NumReportList += 100, 0);
                }
                state.dataOutputProcessor->ReportList(state.dataOutputProcessor->NumExtraVars) = Loop;
            }
        }
    }

    static std::string frequencyNotice([[maybe_unused]] StoreType storeType, ReportingFrequency reportingInterval)
    {
        switch (reportingInterval) {
        case ReportingFrequency::EachCall:
            return " !Each Call";
            break;
        case ReportingFrequency::TimeStep:
            return " !TimeStep";
            break;
        case ReportingFrequency::Hourly:
            return " !Hourly";
            break;
        case ReportingFrequency::Daily:
            return " !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]";
            break;
        case ReportingFrequency::Monthly:
            return " !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]";
            break;
        case ReportingFrequency::Yearly:
            return " !Annual [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]";
            break;
        case ReportingFrequency::Simulation:
            return " !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]";
            break;
        }
        return " !Hourly";
    }

    std::string reportingFrequency(ReportingFrequency reportingInterval)
    {
        switch (reportingInterval) {
        case ReportingFrequency::EachCall:
            return "Each Call";
            break;
        case ReportingFrequency::TimeStep:
            return "TimeStep";
            break;
        case ReportingFrequency::Hourly:
            return "Hourly";
            break;
        case ReportingFrequency::Daily:
            return "Daily";
            break;
        case ReportingFrequency::Monthly:
            return "Monthly";
            break;
        case ReportingFrequency::Yearly:
            return "Annual";
            break;
        case ReportingFrequency::Simulation:
            return "RunPeriod";
            break;
        }
        return "Hourly";
    }

    ReportingFrequency determineFrequency(EnergyPlusData &state, const std::string &FreqString)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       December 2017; Jason DeGraw
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine looks at the passed in report frequency string and
        // determines the reporting frequency.

        // METHODOLOGY EMPLOYED:
        // na

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

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::vector<std::string> const PossibleFreq({"deta", "time", "hour", "dail", "mont", "runp", "envi", "annu"});
        //=(/'detail','Timestep','Hourly','Daily','Monthly','RunPeriod','Environment','Annual'/)
        static std::vector<std::string> const ExactFreqString(
            {"Detailed", "Timestep", "Hourly", "Daily", "Monthly", "RunPeriod", "Environment", "Annual"});
        // Vector of the result, was { -1, 0, 1, 2, 3, 4, 4, 4 } before the addition of Yearly;
        static std::vector<ReportingFrequency> const FreqValues({ReportingFrequency::EachCall,
                                                                 ReportingFrequency::TimeStep,
                                                                 ReportingFrequency::Hourly,
                                                                 ReportingFrequency::Daily,
                                                                 ReportingFrequency::Monthly,
                                                                 ReportingFrequency::Simulation,
                                                                 ReportingFrequency::Simulation,
                                                                 ReportingFrequency::Yearly});
        // note: runperiod and environment are synonomous

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        ReportingFrequency ReportFreq(ReportingFrequency::Hourly); // Default
        std::string::size_type const LenString = min(len(FreqString), static_cast<std::string::size_type>(4u));

        if (LenString < 4u) {
            return ReportFreq;
        }

        std::string const FreqStringTrim(FreqString.substr(0, LenString));
        for (unsigned Loop = 0; Loop < FreqValues.size(); ++Loop) {
            if (UtilityRoutines::SameString(FreqStringTrim, PossibleFreq[Loop])) {
                if (!UtilityRoutines::SameString(FreqString, ExactFreqString[Loop])) {
                    ShowWarningError(state, "DetermineFrequency: Entered frequency=\"" + FreqString + "\" is not an exact match to key strings.");
                    ShowContinueError(state, "Frequency=" + ExactFreqString[Loop] + " will be used.");
                }
                ReportFreq = std::max(FreqValues[Loop], state.dataOutputProcessor->minimumReportFrequency);
                break;
            }
        }
        return ReportFreq;
    }

    void GetReportVariableInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       December 2017; Jason DeGraw
        //       RE-ENGINEERED  na

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

        // Using/Aliasing
        using DataSystemVariables::MinReportFrequency;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int NumAlpha;
        int NumNumbers;
        int IOStat;
        static bool ErrorsFound(false); // If errors detected in input
        std::string cCurrentModuleObject;
        Array1D_string cAlphaArgs(4);
        Array1D_string cAlphaFieldNames(4);
        Array1D_bool lAlphaFieldBlanks(4);
        Array1D<Real64> rNumericArgs(1);
        Array1D_string cNumericFieldNames(1);
        Array1D_bool lNumericFieldBlanks(1);


        // Bail out if the input has already been read in
        if (!state.dataOutputProcessor->GetOutputInputFlag) {
            return;
        }
        state.dataOutputProcessor->GetOutputInputFlag = false;

        // First check environment variable to see of possible override for minimum reporting frequency
        if (MinReportFrequency != "") {
            // Formats
            static constexpr auto Format_800("! <Minimum Reporting Frequency (overriding input value)>, Value, Input Value\n");
            static constexpr auto Format_801(" Minimum Reporting Frequency, {},{}\n");
            state.dataOutputProcessor->minimumReportFrequency = determineFrequency(state, MinReportFrequency);
            print(state.files.eio, Format_800);
            print(state.files.eio, Format_801, frequencyNotice(StoreType::Averaged, state.dataOutputProcessor->minimumReportFrequency), MinReportFrequency);
        }

        cCurrentModuleObject = "Output:Variable";
        state.dataOutputProcessor->NumOfReqVariables = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        state.dataOutputProcessor->ReqRepVars.allocate(state.dataOutputProcessor->NumOfReqVariables);

        for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfReqVariables; ++Loop) {

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            // Check for duplicates?

            state.dataOutputProcessor->ReqRepVars(Loop).Key = cAlphaArgs(1);
            if (state.dataOutputProcessor->ReqRepVars(Loop).Key == "*") {
                state.dataOutputProcessor->ReqRepVars(Loop).Key = std::string();
            }

            std::string::size_type const lbpos = index(cAlphaArgs(2), '['); // Remove Units designation if user put it in
            if (lbpos != std::string::npos) {
                cAlphaArgs(2).erase(lbpos);
            }
            state.dataOutputProcessor->ReqRepVars(Loop).VarName = cAlphaArgs(2);

            state.dataOutputProcessor->ReqRepVars(Loop).frequency = determineFrequency(state, cAlphaArgs(3));

            // Schedule information
            state.dataOutputProcessor->ReqRepVars(Loop).SchedName = cAlphaArgs(4);
            if (not_blank(state.dataOutputProcessor->ReqRepVars(Loop).SchedName)) {
                state.dataOutputProcessor->ReqRepVars(Loop).SchedPtr = GetScheduleIndex(state, state.dataOutputProcessor->ReqRepVars(Loop).SchedName);
                if (state.dataOutputProcessor->ReqRepVars(Loop).SchedPtr == 0) {
                    ShowSevereError(state, "GetReportVariableInput: " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ':' + state.dataOutputProcessor->ReqRepVars(Loop).VarName +
                                    "\" invalid " + cAlphaFieldNames(4) + "=\"" + state.dataOutputProcessor->ReqRepVars(Loop).SchedName + "\" - not found.");
                    ErrorsFound = true;
                }
            } else {
                state.dataOutputProcessor->ReqRepVars(Loop).SchedPtr = 0;
            }

            state.dataOutputProcessor->ReqRepVars(Loop).Used = false;
        }

        if (ErrorsFound) {
            ShowFatalError(state, "GetReportVariableInput:" + cCurrentModuleObject + ": errors in input.");
        }
    }

    void ProduceMinMaxString(std::string &String,                // Current value
                             int const DateValue,                // Date of min/max
                             ReportingFrequency const ReportFreq // Reporting Frequency
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine produces the appropriate min/max string depending
        // on the reporting frequency.

        // METHODOLOGY EMPLOYED:
        // Prior to calling this routine, the basic value string will be
        // produced, but DecodeMonDayHrMin will not have been called.

        // REFERENCES:
        // na

        // Using/Aliasing
        using General::DecodeMonDayHrMin;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr auto DayFormat("{},{:2},{:2}");
        static constexpr auto MonthFormat("{},{:2},{:2},{:2}");
        static constexpr auto EnvrnFormat("{},{:2},{:2},{:2},{:2}");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Mon;
        int Day;
        int Hour;
        int Minute;
        std::string StrOut;

        DecodeMonDayHrMin(DateValue, Mon, Day, Hour, Minute);

        switch (ReportFreq) {
        case ReportingFrequency::Daily:
            StrOut = format(DayFormat, strip(String), Hour, Minute);
            break;
        case ReportingFrequency::Monthly:
            StrOut = format(MonthFormat, strip(String), Day, Hour, Minute);
            break;
        case ReportingFrequency::Yearly:
            StrOut = format(EnvrnFormat, strip(String), Mon, Day, Hour, Minute);
            break;
        case ReportingFrequency::Simulation:
            StrOut = format(EnvrnFormat, strip(String), Mon, Day, Hour, Minute);
            break;
        default: // Each, TimeStep, Hourly dont have this
            StrOut = std::string();
            break;
        }

        String = StrOut;
    }

    void ProduceMinMaxStringWStartMinute(EnergyPlusData &state,
                                         std::string &String,                // Current value
                                         int const DateValue,                // Date of min/max
                                         ReportingFrequency const ReportFreq // Reporting Frequency
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine produces the appropriate min/max string depending
        // on the reporting frequency.  Used in Meter reporting.

        // METHODOLOGY EMPLOYED:
        // Prior to calling this routine, the basic value string will be
        // produced, but DecodeMonDayHrMin will not have been called.  Uses the MinutesPerTimeStep
        // value to set the StartMinute.

        // REFERENCES:
        // na

        // Using/Aliasing
        using General::DecodeMonDayHrMin;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr auto HrFormat("{},{:02}:{:02}");
        static constexpr auto DayFormat("{},{:2},{:02}:{:02}");
        static constexpr auto MonthFormat("{},{:2},{:2},{:02}:{:02}");
        static constexpr auto EnvrnFormat("{},{:2},{:2},{:2},{:02}:{:02}");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Mon;
        int Day;
        int Hour;
        int Minute;
        int StartMinute;
        std::string StrOut;

        DecodeMonDayHrMin(DateValue, Mon, Day, Hour, Minute);

        switch (ReportFreq) {
        case ReportingFrequency::Hourly: // Hourly -- used in meters
            StartMinute = Minute - state.dataGlobal->MinutesPerTimeStep + 1;
            StrOut = format(HrFormat, strip(String), StartMinute, Minute);
            break;

        case ReportingFrequency::Daily: // Daily
            StartMinute = Minute - state.dataGlobal->MinutesPerTimeStep + 1;
            StrOut = format(DayFormat, strip(String), Hour, StartMinute, Minute);
            break;

        case ReportingFrequency::Monthly: // Monthly
            StartMinute = Minute - state.dataGlobal->MinutesPerTimeStep + 1;
            StrOut = format(MonthFormat, strip(String), Day, Hour, StartMinute, Minute);
            break;

        case ReportingFrequency::Yearly: // Yearly
            StartMinute = Minute - state.dataGlobal->MinutesPerTimeStep + 1;
            StrOut = format(EnvrnFormat, strip(String), Mon, Day, Hour, StartMinute, Minute);
            break;

        case ReportingFrequency::Simulation: // Environment
            StartMinute = Minute - state.dataGlobal->MinutesPerTimeStep + 1;
            StrOut = format(EnvrnFormat, strip(String), Mon, Day, Hour, StartMinute, Minute);
            break;

        default: // Each, TimeStep, Hourly dont have this
            StrOut = std::string();
            break;
        }

        String = StrOut;
    }

    TimeStepType ValidateTimeStepType(EnergyPlusData &state, std::string const &TimeStepTypeKey, // Index type (Zone, HVAC) for variables
                                      std::string const &CalledFrom       // Routine called from (for error messages)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function validates the requested "index" type and returns
        // the proper value for use inside the OutputProcessor.

        // METHODOLOGY EMPLOYED:
        // Look it up in a list of valid index types.

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        // TODO: , "HEATBALANCE", "HEAT BALANCE" are used nowhere aside from tests. Should we remove them?
        static std::vector<std::string> zoneIndexes({"ZONE", "HEATBALANCE", "HEAT BALANCE"});
        static std::vector<std::string> systemIndexes({"HVAC", "SYSTEM", "PLANT"});
        std::string uppercase(UtilityRoutines::MakeUPPERCase(TimeStepTypeKey));

        if (std::find(zoneIndexes.begin(), zoneIndexes.end(), uppercase) != zoneIndexes.end()) {
            return TimeStepType::TimeStepZone;
        }

        if (std::find(systemIndexes.begin(), systemIndexes.end(), uppercase) != systemIndexes.end()) {
            return TimeStepType::TimeStepSystem;
        }

        //  The following should never happen to a user!!!!
        ShowSevereError(state, "OutputProcessor/ValidateTimeStepType: Invalid Index Key passed to ValidateTimeStepType=" + TimeStepTypeKey);
        ShowContinueError(state, R"(..Should be "ZONE", "SYSTEM", "HVAC", or "PLANT"... was called from:)" + CalledFrom);
        ShowFatalError(state, "Preceding condition causes termination.");

        return TimeStepType::TimeStepZone;
    }

    std::string StandardTimeStepTypeKey(TimeStepType const timeStepType)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function gives the standard string for the index type
        // given.

        // METHODOLOGY EMPLOYED:
        // Look it up in a list of valid index types.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        std::string StandardTimeStepTypeKey;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        // na

        if (timeStepType == TimeStepType::TimeStepZone) {
            StandardTimeStepTypeKey = "Zone";
        } else if (timeStepType == TimeStepType::TimeStepSystem) {
            StandardTimeStepTypeKey = "HVAC";
        } else {
            StandardTimeStepTypeKey = "UNKW";
        }

        return StandardTimeStepTypeKey;
    }

    StoreType validateVariableType(EnergyPlusData &state, std::string const &VariableTypeKey)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   December 1998
        //       MODIFIED       December 2017; Jason DeGraw
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function validates the VariableTypeKey passed to the SetupVariable
        // routine and assigns it the value used in the OutputProcessor.

        // METHODOLOGY EMPLOYED:
        // Look it up in a list of valid variable types.

        // Return value
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        static Array1D_string StateVariables(3);
        static std::vector<std::string> stateVariables({"STATE", "AVERAGE", "AVERAGED"});
        static Array1D_string NonStateVariables(4);
        static std::vector<std::string> nonStateVariables({"NON STATE", "NONSTATE", "SUM", "SUMMED"});
        std::string uppercase(UtilityRoutines::MakeUPPERCase(VariableTypeKey));

        auto iter = std::find(stateVariables.begin(), stateVariables.end(), uppercase);
        if (iter != stateVariables.end()) {
            return StoreType::Averaged;
        }

        iter = std::find(nonStateVariables.begin(), nonStateVariables.end(), uppercase);
        if (iter != nonStateVariables.end()) {
            return StoreType::Summed;
        }

        ShowSevereError(state, "Invalid variable type requested=" + VariableTypeKey);

        return StoreType::Averaged;
    }

    std::string standardVariableTypeKey(StoreType const VariableType)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   July 1999
        //       MODIFIED       December 2017; Jason DeGraw
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function gives the standard string for the variable type
        // given.

        // METHODOLOGY EMPLOYED:
        // From variable type value, produce proper string.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        // na

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        // na

        switch (VariableType) {
        case StoreType::Averaged:
            return "Average";
            break;
        case StoreType::Summed:
            return "Sum";
            break;
        }

        return "Unknown";
    }

    // *****************************************************************************
    // The following routines implement Energy Meters in EnergyPlus.
    // *****************************************************************************

    void InitializeMeters(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine creates the set of meters in EnergyPlus.  In this initial
        // implementation, it is a static set of meters.

        // METHODOLOGY EMPLOYED:
        // Allocate the static set.  Use "AddMeter" with appropriate arguments that will
        // allow expansion later.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        state.files.mtd.ensure_open(state, "InitializeMeters", state.files.outputControl.mtd);
    }

    void GetCustomMeterInput(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will help implement "custom"/user defined meters.  However, it must be called after all
        // the other meters are set up and all report variables are established.

        // METHODOLOGY EMPLOYED:
        // na

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

        // Using/Aliasing
        using namespace DataIPShortCuts;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;
        int NumNumbers;
        int Loop;
        int IOStat;
        int NumCustomMeters;
        int NumCustomDecMeters;
        int fldIndex;
        bool KeyIsStar;
        Array1D_string NamesOfKeys;                                    // Specific key name
        Array1D_int IndexesForKeyVar;                                  // Array index
        OutputProcessor::Unit UnitsVar(OutputProcessor::Unit::None);   // Units enumeration
        OutputProcessor::Unit MeterUnits(OutputProcessor::Unit::None); // Units enumeration
        int KeyCount;
        int TypeVar;
        OutputProcessor::StoreType AvgSumVar;
        OutputProcessor::TimeStepType StepTypeVar;
        int iKey;
        int iKey1;
        bool MeterCreated;
        Array1D_int VarsOnCustomMeter;
        int MaxVarsOnCustomMeter;
        int NumVarsOnCustomMeter;
        Array1D_int VarsOnSourceMeter;
        int MaxVarsOnSourceMeter;
        int NumVarsOnSourceMeter;
        int iOnMeter;
        int WhichMeter;
        bool errFlag;
        bool BigErrorsFound;
        bool testa;
        bool testb;
        bool Tagged; // variable is appropriate to put on meter
        std::string::size_type lbrackPos;

        BigErrorsFound = false;

        cCurrentModuleObject = "Meter:Custom";
        NumCustomMeters = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        // make list of names for all Meter:Custom since they cannot refer to other Meter:Custom's
        std::unordered_set<std::string> namesOfMeterCustom;
        namesOfMeterCustom.reserve(NumCustomMeters);

        for (Loop = 1; Loop <= NumCustomMeters; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            namesOfMeterCustom.emplace(UtilityRoutines::MakeUPPERCase(cAlphaArgs(1)));
        }

        for (Loop = 1; Loop <= NumCustomMeters; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            lbrackPos = index(cAlphaArgs(1), '[');
            if (lbrackPos != std::string::npos) cAlphaArgs(1).erase(lbrackPos);
            MeterCreated = false;
            if (GlobalNames::VerifyUniqueInterObjectName(state, state.dataOutputProcessor->UniqueMeterNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }
            if (allocated(VarsOnCustomMeter)) VarsOnCustomMeter.deallocate();
            VarsOnCustomMeter.allocate(1000);
            VarsOnCustomMeter = 0;
            MaxVarsOnCustomMeter = 1000;
            NumVarsOnCustomMeter = 0;
            // check if any fields reference another Meter:Custom
            int found = 0;
            for (fldIndex = 4; fldIndex <= NumAlpha; fldIndex += 2) {
                if (namesOfMeterCustom.find(UtilityRoutines::MakeUPPERCase(cAlphaArgs(fldIndex))) != namesOfMeterCustom.end()) {
                    found = fldIndex;
                    break;
                }
            }
            if (found != 0) {
                ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", contains a reference to another " + cCurrentModuleObject +
                                 " in field: " + cAlphaFieldNames(found) + "=\"" + cAlphaArgs(found) + "\".");
                continue;
            }

            for (fldIndex = 3; fldIndex <= NumAlpha; fldIndex += 2) {
                if (cAlphaArgs(fldIndex) == "*" || lAlphaFieldBlanks(fldIndex)) {
                    KeyIsStar = true;
                    cAlphaArgs(fldIndex) = "*";
                } else {
                    KeyIsStar = false;
                }
                if (lAlphaFieldBlanks(fldIndex + 1)) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", blank " + cAlphaFieldNames(fldIndex + 1) + '.');
                    ShowContinueError(state, "...cannot create custom meter.");
                    BigErrorsFound = true;
                    continue;
                }
                if (BigErrorsFound) continue;
                // Don't build/check things out if there were errors anywhere.  Use "GetVariableKeys" to map to actual variables...
                lbrackPos = index(cAlphaArgs(fldIndex + 1), '[');
                if (lbrackPos != std::string::npos) cAlphaArgs(fldIndex + 1).erase(lbrackPos);
                Tagged = false;
                GetVariableKeyCountandType(state, cAlphaArgs(fldIndex + 1), KeyCount, TypeVar, AvgSumVar, StepTypeVar, UnitsVar);
                if (TypeVar == VarType_NotFound) {
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(fldIndex + 1) + "=\"" +
                                     cAlphaArgs(fldIndex + 1) + "\".");
                    ShowContinueError(state, "...will not be shown with the Meter results.");
                    continue;
                }
                if (!MeterCreated) {
                    MeterUnits = UnitsVar; // meter units are same as first variable on custom meter
                    AddMeter(state, cAlphaArgs(1), UnitsVar, std::string(), std::string(), std::string(), std::string());
                    state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).TypeOfMeter = MeterType_Custom;
                    // Can't use resource type in AddMeter cause it will confuse it with other meters.  So, now:
                    GetStandardMeterResourceType(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).ResourceType, UtilityRoutines::MakeUPPERCase(cAlphaArgs(2)), errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "..on " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\".");
                        BigErrorsFound = true;
                    }
                    DetermineMeterIPUnits(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RT_forIPUnits, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).ResourceType, UnitsVar, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "..on " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\".");
                        ShowContinueError(state, "..requests for IP units from this meter will be ignored.");
                    }
                    //        EnergyMeters(NumEnergyMeters)%RT_forIPUnits=DetermineMeterIPUnits(EnergyMeters(NumEnergyMeters)%ResourceType,UnitsVar)
                    MeterCreated = true;
                }
                if (UnitsVar != MeterUnits) {
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", differing units in " + cAlphaFieldNames(fldIndex + 1) +
                                     "=\"" + cAlphaArgs(fldIndex + 1) + "\".");
                    ShowContinueError(state, "...will not be shown with the Meter results; units for meter=" + unitEnumToString(MeterUnits) +
                                      ", units for this variable=" + unitEnumToString(UnitsVar) + '.');
                    continue;
                }
                if ((TypeVar == VarType_Real || TypeVar == VarType_Integer) && AvgSumVar == StoreType::Summed) {
                    Tagged = true;
                    NamesOfKeys.allocate(KeyCount);
                    IndexesForKeyVar.allocate(KeyCount);
                    GetVariableKeys(state, cAlphaArgs(fldIndex + 1), TypeVar, NamesOfKeys, IndexesForKeyVar);
                    iOnMeter = 0;
                    if (KeyIsStar) {
                        for (iKey = 1; iKey <= KeyCount; ++iKey) {
                            ++NumVarsOnCustomMeter;
                            if (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) {
                                VarsOnCustomMeter.redimension(MaxVarsOnCustomMeter += 100, 0);
                            }
                            VarsOnCustomMeter(NumVarsOnCustomMeter) = IndexesForKeyVar(iKey);
                            iOnMeter = 1;
                        }
                        if (iOnMeter == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (all keys) " +
                                            cAlphaFieldNames(fldIndex + 1) + "=\"" + cAlphaArgs(fldIndex + 1) + "\".");
                            ErrorsFound = true;
                        }
                    } else { // Key is not "*"
                        for (iKey = 1; iKey <= KeyCount; ++iKey) {
                            if (NamesOfKeys(iKey) != cAlphaArgs(fldIndex)) continue;
                            ++NumVarsOnCustomMeter;
                            if (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) {
                                VarsOnCustomMeter.redimension(MaxVarsOnCustomMeter += 100, 0);
                            }
                            VarsOnCustomMeter(NumVarsOnCustomMeter) = IndexesForKeyVar(iKey);
                            iOnMeter = 1;
                        }
                        if (iOnMeter == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaArgs(fldIndex) + ':' +
                                            cAlphaArgs(fldIndex + 1));
                            ErrorsFound = true;
                        }
                    }
                    NamesOfKeys.deallocate();
                    IndexesForKeyVar.deallocate();
                }
                if (TypeVar == VarType_Meter && AvgSumVar == StoreType::Summed) {
                    Tagged = true;
                    NamesOfKeys.allocate(KeyCount);
                    IndexesForKeyVar.allocate(KeyCount);
                    GetVariableKeys(state, cAlphaArgs(fldIndex + 1), TypeVar, NamesOfKeys, IndexesForKeyVar);
                    WhichMeter = IndexesForKeyVar(1);
                    NamesOfKeys.deallocate();
                    IndexesForKeyVar.deallocate();
                    // for meters there will only be one key...  but it has variables associated...
                    for (iOnMeter = 1; iOnMeter <= state.dataOutputProcessor->NumVarMeterArrays; ++iOnMeter) {
                        if (!any_eq(state.dataOutputProcessor->VarMeterArrays(iOnMeter).OnMeters, WhichMeter)) continue;
                        ++NumVarsOnCustomMeter;
                        if (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) {
                            VarsOnCustomMeter.redimension(MaxVarsOnCustomMeter += 100, 0);
                        }
                        VarsOnCustomMeter(NumVarsOnCustomMeter) = state.dataOutputProcessor->VarMeterArrays(iOnMeter).RepVariable;
                    }
                }
                if (!Tagged) { // couldn't find place for this item on a meter
                    if (AvgSumVar != StoreType::Summed) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", variable not summed variable " +
                                         cAlphaFieldNames(fldIndex + 1) + "=\"" + cAlphaArgs(fldIndex + 1) + "\".");
                        ShowContinueError(state, "...will not be shown with the Meter results; units for meter=" + unitEnumToString(MeterUnits) +
                                          ", units for this variable=" + unitEnumToString(UnitsVar) + '.');
                    }
                }
            }
            // Check for duplicates
            for (iKey = 1; iKey <= NumVarsOnCustomMeter; ++iKey) {
                if (VarsOnCustomMeter(iKey) == 0) continue;
                for (iKey1 = iKey + 1; iKey1 <= NumVarsOnCustomMeter; ++iKey1) {
                    if (iKey == iKey1) continue;
                    if (VarsOnCustomMeter(iKey) != VarsOnCustomMeter(iKey1)) continue;
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", duplicate name=\"" +
                                     state.dataOutputProcessor->RVariableTypes(VarsOnCustomMeter(iKey1)).VarName + "\".");
                    ShowContinueError(state, "...only one value with this name will be shown with the Meter results.");
                    VarsOnCustomMeter(iKey1) = 0;
                }
            }
            for (iKey = 1; iKey <= NumVarsOnCustomMeter; ++iKey) {
                if (VarsOnCustomMeter(iKey) == 0) continue;
                auto & tmpVar = state.dataOutputProcessor->RVariableTypes(VarsOnCustomMeter(iKey)).VarPtr;
                AttachCustomMeters(state, VarsOnCustomMeter(iKey), tmpVar.MeterArrayPtr, state.dataOutputProcessor->NumEnergyMeters);
            }
            if (NumVarsOnCustomMeter == 0) {
                ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", no items assigned ");
                ShowContinueError(state,
                    "...will not be shown with the Meter results. This may be caused by a Meter:Custom be assigned to another Meter:Custom.");
            }
        }

        cCurrentModuleObject = "Meter:CustomDecrement";
        NumCustomDecMeters = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        for (Loop = 1; Loop <= NumCustomDecMeters; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            lbrackPos = index(cAlphaArgs(1), '[');
            if (lbrackPos != std::string::npos) cAlphaArgs(1).erase(lbrackPos);
            MeterCreated = false;
            if (GlobalNames::VerifyUniqueInterObjectName(state, state.dataOutputProcessor->UniqueMeterNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }
            if (allocated(VarsOnCustomMeter)) VarsOnCustomMeter.deallocate();
            VarsOnCustomMeter.allocate(1000);
            VarsOnCustomMeter = 0;
            MaxVarsOnCustomMeter = 1000;
            NumVarsOnCustomMeter = 0;

            lbrackPos = index(cAlphaArgs(3), '[');
            if (lbrackPos != std::string::npos) cAlphaArgs(1).erase(lbrackPos);
            WhichMeter = UtilityRoutines::FindItem(cAlphaArgs(3), state.dataOutputProcessor->EnergyMeters);
            if (WhichMeter == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                ErrorsFound = true;
                continue;
            }
            //  Set up array of Vars that are on the source meter (for later validation).
            if (allocated(VarsOnSourceMeter)) VarsOnSourceMeter.deallocate();
            VarsOnSourceMeter.allocate(1000);
            VarsOnSourceMeter = 0;
            MaxVarsOnSourceMeter = 1000;
            NumVarsOnSourceMeter = 0;
            for (iKey = 1; iKey <= state.dataOutputProcessor->NumVarMeterArrays; ++iKey) {
                if (state.dataOutputProcessor->VarMeterArrays(iKey).NumOnMeters == 0 && state.dataOutputProcessor->VarMeterArrays(iKey).NumOnCustomMeters == 0) continue;
                //  On a meter
                if (any_eq(state.dataOutputProcessor->VarMeterArrays(iKey).OnMeters, WhichMeter)) {
                    ++NumVarsOnSourceMeter;
                    if (NumVarsOnSourceMeter > MaxVarsOnSourceMeter) {
                        VarsOnSourceMeter.redimension(MaxVarsOnSourceMeter += 100, 0);
                    }
                    VarsOnSourceMeter(NumVarsOnSourceMeter) = state.dataOutputProcessor->VarMeterArrays(iKey).RepVariable;
                    continue;
                }
                if (state.dataOutputProcessor->VarMeterArrays(iKey).NumOnCustomMeters == 0) continue;
                if (any_eq(state.dataOutputProcessor->VarMeterArrays(iKey).OnCustomMeters, WhichMeter)) {
                    ++NumVarsOnSourceMeter;
                    if (NumVarsOnSourceMeter > MaxVarsOnSourceMeter) {
                        VarsOnSourceMeter.redimension(MaxVarsOnSourceMeter += 100, 0);
                    }
                    VarsOnSourceMeter(NumVarsOnSourceMeter) = state.dataOutputProcessor->VarMeterArrays(iKey).RepVariable;
                    continue;
                }
            }

            for (fldIndex = 4; fldIndex <= NumAlpha; fldIndex += 2) {
                if (cAlphaArgs(fldIndex) == "*" || lAlphaFieldBlanks(fldIndex)) {
                    KeyIsStar = true;
                    cAlphaArgs(fldIndex) = "*";
                } else {
                    KeyIsStar = false;
                }
                if (lAlphaFieldBlanks(fldIndex + 1)) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", blank " + cAlphaFieldNames(fldIndex + 1) + '.');
                    ShowContinueError(state, "...cannot create custom meter.");
                    BigErrorsFound = true;
                    continue;
                }
                if (BigErrorsFound) continue;
                Tagged = false;
                lbrackPos = index(cAlphaArgs(fldIndex + 1), '[');
                if (lbrackPos != std::string::npos) cAlphaArgs(fldIndex + 1).erase(lbrackPos);
                // Don't build/check things out if there were errors anywhere.  Use "GetVariableKeys" to map to actual variables...
                GetVariableKeyCountandType(state, cAlphaArgs(fldIndex + 1), KeyCount, TypeVar, AvgSumVar, StepTypeVar, UnitsVar);
                if (TypeVar == VarType_NotFound) {
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(fldIndex + 1) + "=\"" +
                                     cAlphaArgs(fldIndex + 1) + "\".");
                    ShowContinueError(state, "...will not be shown with the Meter results.");
                    continue;
                }
                if (!MeterCreated) {
                    MeterUnits = UnitsVar;
                    AddMeter(state, cAlphaArgs(1), UnitsVar, std::string(), std::string(), std::string(), std::string());
                    state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).TypeOfMeter = MeterType_CustomDec;
                    state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SourceMeter = WhichMeter;

                    // Can't use resource type in AddMeter cause it will confuse it with other meters.  So, now:
                    GetStandardMeterResourceType(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).ResourceType, UtilityRoutines::MakeUPPERCase(cAlphaArgs(2)), errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "..on " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\".");
                        BigErrorsFound = true;
                    }
                    DetermineMeterIPUnits(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RT_forIPUnits, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).ResourceType, UnitsVar, errFlag);
                    if (errFlag) {
                        ShowContinueError(state, "..on " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\".");
                        ShowContinueError(state, "..requests for IP units from this meter will be ignored.");
                    }
                    //        EnergyMeters(NumEnergyMeters)%RT_forIPUnits=DetermineMeterIPUnits(EnergyMeters(NumEnergyMeters)%ResourceType,UnitsVar)
                    MeterCreated = true;
                }
                if (UnitsVar != MeterUnits) {
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", differing units in " + cAlphaFieldNames(fldIndex + 1) +
                                     "=\"" + cAlphaArgs(fldIndex + 1) + "\".");
                    ShowContinueError(state, "...will not be shown with the Meter results; units for meter=" + unitEnumToString(MeterUnits) +
                                      ", units for this variable=" + unitEnumToString(UnitsVar) + '.');
                    continue;
                }
                if ((TypeVar == VarType_Real || TypeVar == VarType_Integer) && AvgSumVar == StoreType::Summed) {
                    Tagged = true;
                    NamesOfKeys.allocate(KeyCount);
                    IndexesForKeyVar.allocate(KeyCount);
                    GetVariableKeys(state, cAlphaArgs(fldIndex + 1), TypeVar, NamesOfKeys, IndexesForKeyVar);
                    iOnMeter = 0;
                    if (KeyIsStar) {
                        for (iKey = 1; iKey <= KeyCount; ++iKey) {
                            ++NumVarsOnCustomMeter;
                            if (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) {
                                VarsOnCustomMeter.redimension(MaxVarsOnCustomMeter += 100, 0);
                            }
                            VarsOnCustomMeter(NumVarsOnCustomMeter) = IndexesForKeyVar(iKey);
                            iOnMeter = 1;
                        }
                        if (iOnMeter == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid (all keys) " +
                                            cAlphaFieldNames(fldIndex + 1) + "=\"" + cAlphaArgs(fldIndex + 1) + "\".");
                            ErrorsFound = true;
                        }
                    } else {
                        for (iKey = 1; iKey <= KeyCount; ++iKey) {
                            if (NamesOfKeys(iKey) != cAlphaArgs(fldIndex)) continue;
                            ++NumVarsOnCustomMeter;
                            if (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) {
                                VarsOnCustomMeter.redimension(MaxVarsOnCustomMeter += 100, 0);
                            }
                            VarsOnCustomMeter(NumVarsOnCustomMeter) = IndexesForKeyVar(iKey);
                            iOnMeter = 1;
                        }
                        if (iOnMeter == 0) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaArgs(fldIndex) + ':' +
                                            cAlphaArgs(fldIndex + 1));
                            ErrorsFound = true;
                        }
                    }
                    NamesOfKeys.deallocate();
                    IndexesForKeyVar.deallocate();
                }
                if (TypeVar == VarType_Meter && AvgSumVar == StoreType::Summed) {
                    Tagged = true;
                    NamesOfKeys.allocate(KeyCount);
                    IndexesForKeyVar.allocate(KeyCount);
                    GetVariableKeys(state, cAlphaArgs(fldIndex + 1), TypeVar, NamesOfKeys, IndexesForKeyVar);
                    WhichMeter = IndexesForKeyVar(1);
                    NamesOfKeys.deallocate();
                    IndexesForKeyVar.deallocate();
                    // for meters there will only be one key...  but it has variables associated...
                    for (iOnMeter = 1; iOnMeter <= state.dataOutputProcessor->NumVarMeterArrays; ++iOnMeter) {
                        testa = any_eq(state.dataOutputProcessor->VarMeterArrays(iOnMeter).OnMeters, WhichMeter);
                        testb = false;
                        if (state.dataOutputProcessor->VarMeterArrays(iOnMeter).NumOnCustomMeters > 0) {
                            testb = any_eq(state.dataOutputProcessor->VarMeterArrays(iOnMeter).OnCustomMeters, WhichMeter);
                        }
                        if (!(testa || testb)) continue;
                        ++NumVarsOnCustomMeter;
                        if (NumVarsOnCustomMeter > MaxVarsOnCustomMeter) {
                            VarsOnCustomMeter.redimension(MaxVarsOnCustomMeter += 100, 0);
                        }
                        VarsOnCustomMeter(NumVarsOnCustomMeter) = state.dataOutputProcessor->VarMeterArrays(iOnMeter).RepVariable;
                    }
                }
                if (!Tagged) { // couldn't find place for this item on a meter
                    if (AvgSumVar != StoreType::Summed) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", variable not summed variable " +
                                         cAlphaFieldNames(fldIndex + 1) + "=\"" + cAlphaArgs(fldIndex + 1) + "\".");
                        ShowContinueError(state, "...will not be shown with the Meter results; units for meter=" + unitEnumToString(MeterUnits) +
                                          ", units for this variable=" + unitEnumToString(UnitsVar) + '.');
                    }
                }
            }
            // Check for duplicates
            for (iKey = 1; iKey <= NumVarsOnCustomMeter; ++iKey) {
                if (VarsOnCustomMeter(iKey) == 0) continue;
                for (iKey1 = iKey + 1; iKey1 <= NumVarsOnCustomMeter; ++iKey1) {
                    if (iKey == iKey1) continue;
                    if (VarsOnCustomMeter(iKey) != VarsOnCustomMeter(iKey1)) continue;
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", duplicate name=\"" +
                                     state.dataOutputProcessor->RVariableTypes(VarsOnCustomMeter(iKey1)).VarName + "\".");
                    ShowContinueError(state, "...only one value with this name will be shown with the Meter results.");
                    VarsOnCustomMeter(iKey1) = 0;
                }
            }
            for (iKey = 1; iKey <= NumVarsOnCustomMeter; ++iKey) {
                if (VarsOnCustomMeter(iKey) == 0) continue;
                auto & tmpVar = state.dataOutputProcessor->RVariableTypes(VarsOnCustomMeter(iKey)).VarPtr;
                AttachCustomMeters(state, VarsOnCustomMeter(iKey), tmpVar.MeterArrayPtr, state.dataOutputProcessor->NumEnergyMeters);
            }

            errFlag = false;
            for (iKey = 1; iKey <= NumVarsOnCustomMeter; ++iKey) {
                for (iKey1 = 1; iKey1 <= NumVarsOnSourceMeter; ++iKey1) {
                    if (any_eq(VarsOnSourceMeter, VarsOnCustomMeter(iKey))) break;
                    if (!errFlag) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid specification to " + cAlphaFieldNames(3) + "=\"" +
                                        cAlphaArgs(3) + "\".");
                        errFlag = true;
                    }
                    ShowContinueError(state, "..Variable=" + state.dataOutputProcessor->RVariableTypes(VarsOnCustomMeter(iKey)).VarName);
                    ErrorsFound = true;
                    break;
                }
            }
            if (NumVarsOnCustomMeter == 0) {
                ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", no items assigned ");
                ShowContinueError(state, "...will not be shown with the Meter results");
            }

            VarsOnCustomMeter.deallocate();
            VarsOnSourceMeter.deallocate();
        }

        if (BigErrorsFound) ErrorsFound = true;
    }

    void GetStandardMeterResourceType(EnergyPlusData &state, std::string &OutResourceType,
                                      std::string const &UserInputResourceType, // Passed uppercase
                                      bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine compares the user input resource type with valid ones and returns
        // the standard resource type.

        ErrorsFound = false;

        // Basic ResourceType for Meters
        {
            auto const meterType(UserInputResourceType);

            if (meterType == "ELECTRICITY") {
                OutResourceType = "Electricity";

            } else if (meterType == "NATURALGAS") {
                OutResourceType = "NaturalGas";

            } else if (meterType == "GASOLINE") {
                OutResourceType = "Gasoline";

            } else if (meterType == "DIESEL") {
                OutResourceType = "Diesel";

            } else if (meterType == "COAL") {
                OutResourceType = "Coal";

            } else if (meterType == "FUELOILNO1") {
                OutResourceType = "FuelOilNo1";

            } else if (meterType == "FUELOILNO2") {
                OutResourceType = "FuelOilNo2";

            } else if (meterType == "PROPANE") {
                OutResourceType = "Propane";

            } else if (meterType == "WATER" || meterType == "H2O") {
                OutResourceType = "Water"; // this is water "use"

            } else if (meterType == "ONSITEWATER" || meterType == "WATERPRODUCED" || meterType == "ONSITE WATER") {
                OutResourceType = "OnSiteWater"; // these are for supply record keeping

            } else if (meterType == "MAINSWATER" || meterType == "WATERSUPPLY") {
                OutResourceType = "MainsWater"; // record keeping

            } else if (meterType == "RAINWATER" || meterType == "PRECIPITATION") {
                OutResourceType = "RainWater"; // record keeping

            } else if (meterType == "WELLWATER" || meterType == "GROUNDWATER") {
                OutResourceType = "WellWater"; // record keeping

            } else if (meterType == "CONDENSATE") {
                OutResourceType = "Condensate"; // record keeping

            } else if (meterType == "ENERGYTRANSFER" || meterType == "ENERGYXFER" || meterType == "XFER") {
                OutResourceType = "EnergyTransfer";

            } else if (meterType == "STEAM") {
                OutResourceType = "Steam";

            } else if (meterType == "DISTRICTCOOLING") {
                OutResourceType = "DistrictCooling";

            } else if (meterType == "DISTRICTHEATING") {
                OutResourceType = "DistrictHeating";

            } else if (meterType == "ELECTRICITYPRODUCED") {
                OutResourceType = "ElectricityProduced";

            } else if (meterType == "ELECTRICITYPURCHASED") {
                OutResourceType = "ElectricityPurchased";

            } else if (meterType == "ELECTRICITYSURPLUSSOLD") {
                OutResourceType = "ElectricitySurplusSold";

            } else if (meterType == "ELECTRICITYNET") {
                OutResourceType = "ElectricityNet";

            } else if (meterType == "SOLARWATER") {
                OutResourceType = "SolarWater";

            } else if (meterType == "SOLARAIR") {
                OutResourceType = "SolarAir";

            } else if (meterType == "SO2") {
                OutResourceType = "SO2";

            } else if (meterType == "NOX") {
                OutResourceType = "NOx";

            } else if (meterType == "N2O") {
                OutResourceType = "N2O";

            } else if (meterType == "PM") {
                OutResourceType = "PM";

            } else if (meterType == "PM2.5") {
                OutResourceType = "PM2.5";

            } else if (meterType == "PM10") {
                OutResourceType = "PM10";

            } else if (meterType == "CO") {
                OutResourceType = "CO";

            } else if (meterType == "CO2") {
                OutResourceType = "CO2";

            } else if (meterType == "CH4") {
                OutResourceType = "CH4";

            } else if (meterType == "NH3") {
                OutResourceType = "NH3";

            } else if (meterType == "NMVOC") {
                OutResourceType = "NMVOC";

            } else if (meterType == "HG") {
                OutResourceType = "Hg";

            } else if (meterType == "PB") {
                OutResourceType = "Pb";

            } else if (meterType == "NUCLEAR HIGH") {
                OutResourceType = "Nuclear High";

            } else if (meterType == "NUCLEAR LOW") {
                OutResourceType = "Nuclear Low";

            } else if (meterType == "WATERENVIRONMENTALFACTORS") {
                OutResourceType = "WaterEnvironmentalFactors";

            } else if (meterType == "CARBON EQUIVALENT") {
                OutResourceType = "Carbon Equivalent";

            } else if (meterType == "SOURCE") {
                OutResourceType = "Source";

            } else if (meterType == "PLANTLOOPHEATINGDEMAND") {
                OutResourceType = "PlantLoopHeatingDemand";

            } else if (meterType == "PLANTLOOPCOOLINGDEMAND") {
                OutResourceType = "PlantLoopCoolingDemand";

            } else if (meterType == "GENERIC") { // only used by custom meters
                OutResourceType = "Generic";

            } else if (meterType == "OTHERFUEL1") { // other fuel type (defined by user)
                OutResourceType = "OtherFuel1";

            } else if (meterType == "OTHERFUEL2") { // other fuel type (defined by user)
                OutResourceType = "OtherFuel2";

            } else {
                ShowSevereError(state, "GetStandardMeterResourceType: Illegal OutResourceType (for Meters) Entered=" + UserInputResourceType);
                ErrorsFound = true;
            }
        }
    }

    void AddMeter(EnergyPlusData &state, std::string const &Name,               // Name for the meter
                  OutputProcessor::Unit const &MtrUnits, // Units for the meter
                  std::string const &ResourceType,       // ResourceType for the meter
                  std::string const &EndUse,             // EndUse for the meter
                  std::string const &EndUseSub,          // EndUse subcategory for the meter
                  std::string const &Group               // Group for the meter
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine adds a meter to the current definition set of meters.  If the maximum has
        // already been reached, a reallocation procedure begins.  This action needs to be done at the
        // start of the simulation, primarily before any output is stored.

        // Make sure this isn't already in the list of meter names
        int Found;
        if (state.dataOutputProcessor->NumEnergyMeters > 0) {
            Found = UtilityRoutines::FindItemInList(Name, state.dataOutputProcessor->EnergyMeters);
        } else {
            Found = 0;
        }

        if (Found == 0) {
            state.dataOutputProcessor->EnergyMeters.redimension(++state.dataOutputProcessor->NumEnergyMeters);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).Name = Name;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).ResourceType = ResourceType;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).EndUse = EndUse;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).EndUseSub = EndUseSub;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).Group = Group;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).Units = MtrUnits;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).TSValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).CurTSValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptTS = false;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptTSFO = false;
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).TSRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).TSRptNumChr = fmt::to_string(state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).TSRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).HRValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).HRMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).HRMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).HRMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).HRMinValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptHR = false;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptHRFO = false;
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).HRRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).HRRptNumChr = fmt::to_string(state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).HRRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).DYValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).DYMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).DYMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).DYMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).DYMinValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptDY = false;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptDYFO = false;
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).DYRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).DYRptNumChr = fmt::to_string(state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).DYRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).MNValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).MNMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).MNMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).MNMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).MNMinValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptMN = false;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptMNFO = false;
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).MNRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).MNRptNumChr = fmt::to_string(state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).MNRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).YRValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).YRMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).YRMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).YRMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).YRMinValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptYR = false;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptYRFO = false;
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).YRRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).YRRptNumChr = fmt::to_string(state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).YRRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SMValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SMMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SMMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SMMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SMMinValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptSM = false;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RptSMFO = false;
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SMRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SMRptNumChr = fmt::to_string(state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SMRptNum);
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).TSAccRptNum);
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).HRAccRptNum);
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).DYAccRptNum);
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).MNAccRptNum);
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).YRAccRptNum);
            AssignReportNumber(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).SMAccRptNum);
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).FinYrSMValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).FinYrSMMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).FinYrSMMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).FinYrSMMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).FinYrSMMinValDate = 0;
        } else {
            ShowFatalError(state, "Requested to Add Meter which was already present=" + Name);
        }
        if (!ResourceType.empty()) {
            bool errFlag;
            DetermineMeterIPUnits(state, state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->NumEnergyMeters).RT_forIPUnits, ResourceType, MtrUnits, errFlag);
            if (errFlag) {
                ShowContinueError(state, "..on Meter=\"" + Name + "\".");
                ShowContinueError(state, "..requests for IP units from this meter will be ignored.");
            }
        }
    }

    void AttachMeters(EnergyPlusData &state, OutputProcessor::Unit const &MtrUnits, // Units for this meter
                      std::string &ResourceType,             // Electricity, Gas, etc.
                      std::string &EndUse,                   // End-use category (Lights, Heating, etc.)
                      std::string &EndUseSub,                // End-use subcategory (user-defined, e.g., General Lights, Task Lights, etc.)
                      std::string &Group,                    // Group key (Facility, Zone, Building, etc.)
                      std::string const &ZoneName,           // Zone key only applicable for Building group
                      int const RepVarNum,                   // Number of this report variable
                      int &MeterArrayPtr,                    // Output set of Pointers to Meters
                      bool &ErrorsFound                      // True if errors in this call
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines which meters this variable will be on (if any),
        // sets up the meter pointer arrays, and returns a index value to this array which
        // is stored with the variable.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        if (UtilityRoutines::SameString(Group, "Building")) {
            ValidateNStandardizeMeterTitles(state, MtrUnits, ResourceType, EndUse, EndUseSub, Group, ErrorsFound, ZoneName);
        } else {
            ValidateNStandardizeMeterTitles(state, MtrUnits, ResourceType, EndUse, EndUseSub, Group, ErrorsFound);
        }

        state.dataOutputProcessor->VarMeterArrays.redimension(++state.dataOutputProcessor->NumVarMeterArrays);
        MeterArrayPtr = state.dataOutputProcessor->NumVarMeterArrays;
        state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters = 0;
        state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).RepVariable = RepVarNum;
        state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnMeters = 0;
        int Found = UtilityRoutines::FindItem(ResourceType + ":Facility", state.dataOutputProcessor->EnergyMeters);
        if (Found != 0) {
            ++state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters;
            state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnMeters(state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters) = Found;
        }
        if (!Group.empty()) {
            Found = UtilityRoutines::FindItem(ResourceType + ':' + Group, state.dataOutputProcessor->EnergyMeters);
            if (Found != 0) {
                ++state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters;
                state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnMeters(state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters) = Found;
            }
            if (UtilityRoutines::SameString(Group, "Building")) { // Match to Zone
                Found = UtilityRoutines::FindItem(ResourceType + ":Zone:" + ZoneName, state.dataOutputProcessor->EnergyMeters);
                if (Found != 0) {
                    ++state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters;
                    state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnMeters(state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters) = Found;
                }
            }
        }

        //!! Following if EndUse is by ResourceType
        if (!EndUse.empty()) {
            Found = UtilityRoutines::FindItem(EndUse + ':' + ResourceType, state.dataOutputProcessor->EnergyMeters);
            if (Found != 0) {
                ++state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters;
                state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnMeters(state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters) = Found;
            }
            if (UtilityRoutines::SameString(Group, "Building")) { // Match to Zone
                Found = UtilityRoutines::FindItem(EndUse + ':' + ResourceType + ":Zone:" + ZoneName, state.dataOutputProcessor->EnergyMeters);
                if (Found != 0) {
                    ++state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters;
                    state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnMeters(state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters) = Found;
                }
            }

            // End use subcategory
            if (!EndUseSub.empty()) {
                Found = UtilityRoutines::FindItem(EndUseSub + ':' + EndUse + ':' + ResourceType, state.dataOutputProcessor->EnergyMeters);
                if (Found != 0) {
                    ++state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters;
                    state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnMeters(state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters) = Found;

                    AddEndUseSubcategory(state, ResourceType, EndUse, EndUseSub);
                }
                if (UtilityRoutines::SameString(Group, "Building")) { // Match to Zone
                    Found = UtilityRoutines::FindItem(EndUseSub + ':' + EndUse + ':' + ResourceType + ":Zone:" + ZoneName, state.dataOutputProcessor->EnergyMeters);
                    if (Found != 0) {
                        ++state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters;
                        state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnMeters(state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters) = Found;
                    }
                }
            }
        }
    }

    void AttachCustomMeters(EnergyPlusData &state,
                            int const RepVarNum, // Number of this report variable
                            int &MeterArrayPtr,  // Input/Output set of Pointers to Meters
                            int const MeterIndex // Which meter this is
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines which meters this variable will be on (if any),
        // sets up the meter pointer arrays, and returns a index value to this array which
        // is stored with the variable.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        if (MeterArrayPtr == 0) {
            state.dataOutputProcessor->VarMeterArrays.redimension(++state.dataOutputProcessor->NumVarMeterArrays);
            MeterArrayPtr = state.dataOutputProcessor->NumVarMeterArrays;
            state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnMeters = 0;
            state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).RepVariable = RepVarNum;
            state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnMeters = 0;
            state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).OnCustomMeters.allocate(1);
            state.dataOutputProcessor->VarMeterArrays(state.dataOutputProcessor->NumVarMeterArrays).NumOnCustomMeters = 1;
        } else { // MeterArrayPtr set
            state.dataOutputProcessor->VarMeterArrays(MeterArrayPtr).OnCustomMeters.redimension(++state.dataOutputProcessor->VarMeterArrays(MeterArrayPtr).NumOnCustomMeters);
        }
        state.dataOutputProcessor->VarMeterArrays(MeterArrayPtr).OnCustomMeters(state.dataOutputProcessor->VarMeterArrays(MeterArrayPtr).NumOnCustomMeters) = MeterIndex;
    }

    void ValidateNStandardizeMeterTitles(EnergyPlusData &state, OutputProcessor::Unit const &MtrUnits, // Units for the meter
                                         std::string &ResourceType,             // Electricity, Gas, etc.
                                         std::string &EndUse,                   // End Use Type (Lights, Heating, etc.)
                                         std::string &EndUseSub,                // End Use Sub Type (General Lights, Task Lights, etc.)
                                         std::string &Group,                    // Group key (Facility, Zone, Building, etc.)
                                         bool &ErrorsFound,                     // True if errors in this call
                                         Optional_string_const ZoneName         // ZoneName when Group=Building
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine uses the keys for the Energy Meters given to the SetupOutputVariable routines
        // and makes sure they are "standard" as well as creating meters which need to be added as this
        // is the first use of that kind of meter designation.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Found; // For checking whether meter is already defined
        bool LocalErrorsFound;
        std::string MeterName;

        LocalErrorsFound = false;
        // Basic ResourceType Meters
        GetStandardMeterResourceType(state, ResourceType, UtilityRoutines::MakeUPPERCase(ResourceType), LocalErrorsFound);

        if (!LocalErrorsFound) {
            if (state.dataOutputProcessor->NumEnergyMeters > 0) {
                Found = UtilityRoutines::FindItem(ResourceType + ":Facility", state.dataOutputProcessor->EnergyMeters);
            } else {
                Found = 0;
            }
            if (Found == 0) AddMeter(state, ResourceType + ":Facility", MtrUnits, ResourceType, "", "", "");
        }

        //!  Group Meters
        {
            auto const groupMeter(uppercased(Group));

            if (groupMeter.empty()) {

            } else if (groupMeter == "BUILDING") {
                Group = "Building";

            } else if (groupMeter == "HVAC" || groupMeter == "SYSTEM") {
                Group = "HVAC";

            } else if (groupMeter == "PLANT") {
                Group = "Plant";

            } else {
                ShowSevereError(state, "Illegal Group (for Meters) Entered=" + Group);
                LocalErrorsFound = true;
            }
        }

        if (!LocalErrorsFound && !Group.empty()) {
            Found = UtilityRoutines::FindItem(ResourceType + ':' + Group, state.dataOutputProcessor->EnergyMeters);
            if (Found == 0) AddMeter(state, ResourceType + ':' + Group, MtrUnits, ResourceType, "", "", Group);
            if (Group == "Building") {
                Found = UtilityRoutines::FindItem(ResourceType + ":Zone:" + ZoneName, state.dataOutputProcessor->EnergyMeters);
                if (Found == 0) {
                    AddMeter(state, ResourceType + ":Zone:" + ZoneName, MtrUnits, ResourceType, "", "", "Zone");
                }
            }
        }

        //!!! EndUse Meters
        {
            auto const endUseMeter(uppercased(EndUse));

            if (endUseMeter.empty()) {

            } else if (endUseMeter == "INTERIOR LIGHTS" || endUseMeter == "INTERIORLIGHTS") {
                EndUse = "InteriorLights";

            } else if (endUseMeter == "EXTERIOR LIGHTS" || endUseMeter == "EXTERIORLIGHTS") {
                EndUse = "ExteriorLights";

            } else if (endUseMeter == "HEATING" || endUseMeter == "HTG") {
                EndUse = "Heating";

            } else if (endUseMeter == "HEATPRODUCED") {
                EndUse = "HeatProduced";

            } else if (endUseMeter == "COOLING" || endUseMeter == "CLG") {
                EndUse = "Cooling";

            } else if (endUseMeter == "DOMESTICHOTWATER" || endUseMeter == "DHW" || endUseMeter == "DOMESTIC HOT WATER") {
                EndUse = "WaterSystems";

            } else if (endUseMeter == "COGEN" || endUseMeter == "COGENERATION") {
                EndUse = "Cogeneration";

            } else if (endUseMeter == "INTERIOREQUIPMENT" || endUseMeter == "INTERIOR EQUIPMENT") {
                EndUse = "InteriorEquipment";

            } else if (endUseMeter == "EXTERIOREQUIPMENT" || endUseMeter == "EXTERIOR EQUIPMENT" || endUseMeter == "EXT EQ" ||
                       endUseMeter == "EXTERIOREQ") {
                EndUse = "ExteriorEquipment";

            } else if (endUseMeter == "EXTERIOR:WATEREQUIPMENT") {
                EndUse = "ExteriorEquipment";

            } else if (endUseMeter == "PURCHASEDHOTWATER" || endUseMeter == "DISTRICTHOTWATER" || endUseMeter == "PURCHASED HEATING") {
                EndUse = "DistrictHotWater";

            } else if (endUseMeter == "PURCHASEDCOLDWATER" || endUseMeter == "DISTRICTCHILLEDWATER" || endUseMeter == "PURCHASEDCHILLEDWATER" ||
                       endUseMeter == "PURCHASED COLD WATER" || endUseMeter == "PURCHASED COOLING") {
                EndUse = "DistrictChilledWater";

            } else if (endUseMeter == "FANS" || endUseMeter == "FAN") {
                EndUse = "Fans";

            } else if (endUseMeter == "HEATINGCOILS" || endUseMeter == "HEATINGCOIL" || endUseMeter == "HEATING COILS" ||
                       endUseMeter == "HEATING COIL") {
                EndUse = "HeatingCoils";

            } else if (endUseMeter == "COOLINGCOILS" || endUseMeter == "COOLINGCOIL" || endUseMeter == "COOLING COILS" ||
                       endUseMeter == "COOLING COIL") {
                EndUse = "CoolingCoils";

            } else if (endUseMeter == "PUMPS" || endUseMeter == "PUMP") {
                EndUse = "Pumps";

            } else if (endUseMeter == "FREECOOLING" || endUseMeter == "FREE COOLING") {
                EndUse = "Freecooling";

            } else if (endUseMeter == "LOOPTOLOOP") {
                EndUse = "LoopToLoop";

            } else if (endUseMeter == "CHILLERS" || endUseMeter == "CHILLER") {
                EndUse = "Chillers";

            } else if (endUseMeter == "BOILERS" || endUseMeter == "BOILER") {
                EndUse = "Boilers";

            } else if (endUseMeter == "BASEBOARD" || endUseMeter == "BASEBOARDS") {
                EndUse = "Baseboard";

            } else if (endUseMeter == "COOLINGPANEL" || endUseMeter == "COOLINGPANELS") {
                EndUse = "CoolingPanel";

            } else if (endUseMeter == "HEATREJECTION" || endUseMeter == "HEAT REJECTION") {
                EndUse = "HeatRejection";

            } else if (endUseMeter == "HUMIDIFIER" || endUseMeter == "HUMIDIFIERS") {
                EndUse = "Humidifier";

            } else if (endUseMeter == "HEATRECOVERY" || endUseMeter == "HEAT RECOVERY") {
                EndUse = "HeatRecovery";

            } else if (endUseMeter == "PHOTOVOLTAICS" || endUseMeter == "PV" || endUseMeter == "PHOTOVOLTAIC") {
                EndUse = "Photovoltaic";

            } else if (endUseMeter == "WINDTURBINES" || endUseMeter == "WT" || endUseMeter == "WINDTURBINE") {
                EndUse = "WindTurbine";

            } else if (endUseMeter == "ELECTRICSTORAGE") {
                EndUse = "ElectricStorage";

            } else if (endUseMeter == "POWERCONVERSION") {

                EndUse = "PowerConversion";

            } else if (endUseMeter == "HEAT RECOVERY FOR COOLING" || endUseMeter == "HEATRECOVERYFORCOOLING" ||
                       endUseMeter == "HEATRECOVERYCOOLING") {
                EndUse = "HeatRecoveryForCooling";

            } else if (endUseMeter == "HEAT RECOVERY FOR HEATING" || endUseMeter == "HEATRECOVERYFORHEATING" ||
                       endUseMeter == "HEATRECOVERYHEATING") {
                EndUse = "HeatRecoveryForHeating";

            } else if (endUseMeter == "ELECTRICEMISSIONS") {
                EndUse = "ElectricEmissions";

            } else if (endUseMeter == "PURCHASEDELECTRICEMISSIONS") {
                EndUse = "PurchasedElectricEmissions";

            } else if (endUseMeter == "SOLDELECTRICEMISSIONS") {
                EndUse = "SoldElectricEmissions";

            } else if (endUseMeter == "NATURALGASEMISSIONS") {
                EndUse = "NaturalGasEmissions";

            } else if (endUseMeter == "FUELOILNO1EMISSIONS") {
                EndUse = "FuelOilNo1Emissions";

            } else if (endUseMeter == "FUELOILNO2EMISSIONS") {
                EndUse = "FuelOilNo2Emissions";

            } else if (endUseMeter == "COALEMISSIONS") {
                EndUse = "CoalEmissions";

            } else if (endUseMeter == "GASOLINEEMISSIONS") {
                EndUse = "GasolineEmissions";

            } else if (endUseMeter == "PROPANEEMISSIONS") {
                EndUse = "PropaneEmissions";

            } else if (endUseMeter == "DIESELEMISSIONS") {
                EndUse = "DieselEmissions";

            } else if (endUseMeter == "OTHERFUEL1EMISSIONS") {
                EndUse = "OtherFuel1Emissions";

            } else if (endUseMeter == "OTHERFUEL2EMISSIONS") {
                EndUse = "OtherFuel2Emissions";

            } else if (endUseMeter == "CARBONEQUIVALENTEMISSIONS") {
                EndUse = "CarbonEquivalentEmissions";

            } else if (endUseMeter == "REFRIGERATION") {
                EndUse = "Refrigeration";

            } else if (endUseMeter == "COLDSTORAGECHARGE") {
                EndUse = "ColdStorageCharge";

            } else if (endUseMeter == "COLDSTORAGEDISCHARGE") {
                EndUse = "ColdStorageDischarge";

            } else if (endUseMeter == "WATERSYSTEMS" || endUseMeter == "WATERSYSTEM" || endUseMeter == "Water System") {
                EndUse = "WaterSystems";

            } else if (endUseMeter == "RAINWATER") {
                EndUse = "Rainwater";

            } else if (endUseMeter == "CONDENSATE") {
                EndUse = "Condensate";

            } else if (endUseMeter == "WELLWATER") {
                EndUse = "Wellwater";

            } else if (endUseMeter == "MAINSWATER" || endUseMeter == "PURCHASEDWATER") {
                EndUse = "MainsWater";

            } else {
                ShowSevereError(state, "Illegal EndUse (for Meters) Entered=" + EndUse);
                LocalErrorsFound = true;
            }
        }

        //!! Following if we do EndUse by ResourceType
        if (!LocalErrorsFound && !EndUse.empty()) {
            Found = UtilityRoutines::FindItem(EndUse + ':' + ResourceType, state.dataOutputProcessor->EnergyMeters);
            if (Found == 0) AddMeter(state, EndUse + ':' + ResourceType, MtrUnits, ResourceType, EndUse, "", "");

            if (Group == "Building") { // Match to Zone
                Found = UtilityRoutines::FindItem(EndUse + ':' + ResourceType + ":Zone:" + ZoneName, state.dataOutputProcessor->EnergyMeters);
                if (Found == 0) {
                    AddMeter(state, EndUse + ':' + ResourceType + ":Zone:" + ZoneName, MtrUnits, ResourceType, EndUse, "", "Zone");
                }
            }
        } else if (LocalErrorsFound) {
            ErrorsFound = true;
        }

        // End-Use Subcategories
        if (!LocalErrorsFound && !EndUseSub.empty()) {
            MeterName = EndUseSub + ':' + EndUse + ':' + ResourceType;
            Found = UtilityRoutines::FindItem(MeterName, state.dataOutputProcessor->EnergyMeters);
            if (Found == 0) AddMeter(state, MeterName, MtrUnits, ResourceType, EndUse, EndUseSub, "");
        } else if (LocalErrorsFound) {
            ErrorsFound = true;
        }
    }

    void DetermineMeterIPUnits(EnergyPlusData &state, int &CodeForIPUnits,                   // Output Code for IP Units
                               std::string const &ResourceType,       // Resource Type
                               OutputProcessor::Unit const &MtrUnits, // Meter units
                               bool &ErrorsFound                      // true if errors found during subroutine
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
        std::string UC_ResourceType;

        ErrorsFound = false;
        UC_ResourceType = UtilityRoutines::MakeUPPERCase(ResourceType);

        CodeForIPUnits = RT_IPUnits_OtherJ;
        if (has(UC_ResourceType, "ELEC")) {
            CodeForIPUnits = RT_IPUnits_Electricity;
        } else if (has(UC_ResourceType, "GAS")) {
            CodeForIPUnits = RT_IPUnits_Gas;
        } else if (has(UC_ResourceType, "COOL")) {
            CodeForIPUnits = RT_IPUnits_Cooling;
        }
        if (MtrUnits == OutputProcessor::Unit::m3 && has(UC_ResourceType, "WATER")) {
            CodeForIPUnits = RT_IPUnits_Water;
        } else if (MtrUnits == OutputProcessor::Unit::m3) {
            CodeForIPUnits = RT_IPUnits_OtherM3;
        }
        if (MtrUnits == OutputProcessor::Unit::kg) {
            CodeForIPUnits = RT_IPUnits_OtherKG;
        }
        if (MtrUnits == OutputProcessor::Unit::L) {
            CodeForIPUnits = RT_IPUnits_OtherL;
        }
        //  write(outputfiledebug,*) 'resourcetype=',TRIM(resourcetype)
        //  write(outputfiledebug,*) 'ipunits type=',CodeForIPUnits
        if (!(MtrUnits == OutputProcessor::Unit::kg) && !(MtrUnits == OutputProcessor::Unit::J) && !(MtrUnits == OutputProcessor::Unit::m3) &&
            !(MtrUnits == OutputProcessor::Unit::L)) {
            ShowWarningError(state, "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[" + unitEnumToString(MtrUnits) + "].");
            ErrorsFound = true;
        }
    }

    void UpdateMeterValues(EnergyPlusData &state,
                           Real64 const TimeStepValue,                // Value of this variable at the current time step.
                           int const NumOnMeters,                     // Number of meters this variable is "on".
                           const Array1D_int &OnMeters                // Which meters this variable is on (index values)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       Jason DeGraw 2/12/2020, de-optionalized
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates all the meter values in the lists with the current
        // time step value for this variable.

        // METHODOLOGY EMPLOYED:
        // Variables, as they are "setup", may or may not be on one or more meters.
        // All "metered" variables are on the "facility meter".  Index values will be
        // set from the variables to the appropriate meters.  Then, the updating of
        // the meter values is quite simple -- just add the time step value of the variable
        // (which is passed to this routine) to all the values being kept for the meter.
        // Reporting of the meters is taken care of in a different routine.  During reporting,
        // some values will also be reset (for example, after reporting the "hour", the new
        // "hour" value of the meter is reset to 0.0, etc.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Meter; // Loop Control
        int Which; // Index value for the meter

        for (Meter = 1; Meter <= NumOnMeters; ++Meter) {
            Which = OnMeters(Meter);
            state.dataOutputProcessor->MeterValue(Which) += TimeStepValue;
        }

    }

    void UpdateMeterValues(EnergyPlusData &state,
                           Real64 const TimeStepValue,                // Value of this variable at the current time step.
                           int const NumOnMeters,                     // Number of meters this variable is "on".
                           const Array1D_int &OnMeters,                // Which meters this variable is on (index values)
                           int const NumOnCustomMeters,               // Number of custom meters this variable is "on".
                           const Array1D_int &OnCustomMeters // Which custom meters this variable is on (index values)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       Jason DeGraw 2/12/2020, de-optionalized
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates all the meter values in the lists with the current
        // time step value for this variable.

        // METHODOLOGY EMPLOYED:
        // Variables, as they are "setup", may or may not be on one or more meters.
        // All "metered" variables are on the "facility meter".  Index values will be
        // set from the variables to the appropriate meters.  Then, the updating of
        // the meter values is quite simple -- just add the time step value of the variable
        // (which is passed to this routine) to all the values being kept for the meter.
        // Reporting of the meters is taken care of in a different routine.  During reporting,
        // some values will also be reset (for example, after reporting the "hour", the new
        // "hour" value of the meter is reset to 0.0, etc.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Meter; // Loop Control
        int Which; // Index value for the meter

        for (Meter = 1; Meter <= NumOnMeters; ++Meter) {
            Which = OnMeters(Meter);
            state.dataOutputProcessor->MeterValue(Which) += TimeStepValue;
        }

        // This calculates the basic values for decrement/difference meters -- UpdateMeters then calculates the actual.
        for (Meter = 1; Meter <= NumOnCustomMeters; ++Meter) {
            Which = OnCustomMeters(Meter);
            state.dataOutputProcessor->MeterValue(Which) += TimeStepValue;
        }
    }

    void UpdateMeters(EnergyPlusData &state, int const TimeStamp) // Current TimeStamp (for max/min)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the meters with the current time step value
        // for each meter.  Also, sets min/max values for hourly...run period reporting.

        // METHODOLOGY EMPLOYED:
        // Goes thru the number of meters, setting min/max as appropriate.  Uses timestamp
        // from calling program.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Meter; // Loop Control

        for (Meter = 1; Meter <= state.dataOutputProcessor->NumEnergyMeters; ++Meter) {
            if (state.dataOutputProcessor->EnergyMeters(Meter).TypeOfMeter != MeterType_CustomDec && state.dataOutputProcessor->EnergyMeters(Meter).TypeOfMeter != MeterType_CustomDiff) {
                state.dataOutputProcessor->EnergyMeters(Meter).TSValue += state.dataOutputProcessor->MeterValue(Meter);
                state.dataOutputProcessor->EnergyMeters(Meter).HRValue += state.dataOutputProcessor->MeterValue(Meter);
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).HRMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).HRMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).HRMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).HRMinValDate);
                state.dataOutputProcessor->EnergyMeters(Meter).DYValue += state.dataOutputProcessor->MeterValue(Meter);
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).DYMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).DYMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).DYMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).DYMinValDate);
                state.dataOutputProcessor->EnergyMeters(Meter).MNValue += state.dataOutputProcessor->MeterValue(Meter);
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).MNMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).MNMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).MNMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).MNMinValDate);
                state.dataOutputProcessor->EnergyMeters(Meter).YRValue += state.dataOutputProcessor->MeterValue(Meter);
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).YRMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).YRMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).YRMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).YRMinValDate);
                state.dataOutputProcessor->EnergyMeters(Meter).SMValue += state.dataOutputProcessor->MeterValue(Meter);
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).SMMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).SMMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).SMMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).SMMinValDate);
                if (state.dataOutputProcessor->isFinalYear) {
                    state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMValue += state.dataOutputProcessor->MeterValue(Meter);
                    SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                              TimeStamp,
                              state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMaxVal,
                              state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMaxValDate,
                              state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMinVal,
                              state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMinValDate);
                }
            } else {
                state.dataOutputProcessor->EnergyMeters(Meter).TSValue = state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->EnergyMeters(Meter).SourceMeter).TSValue - state.dataOutputProcessor->MeterValue(Meter);
                state.dataOutputProcessor->EnergyMeters(Meter).HRValue += state.dataOutputProcessor->EnergyMeters(Meter).TSValue;
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).HRMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).HRMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).HRMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).HRMinValDate);
                state.dataOutputProcessor->EnergyMeters(Meter).DYValue += state.dataOutputProcessor->EnergyMeters(Meter).TSValue;
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).DYMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).DYMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).DYMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).DYMinValDate);
                state.dataOutputProcessor->EnergyMeters(Meter).MNValue += state.dataOutputProcessor->EnergyMeters(Meter).TSValue;
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).MNMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).MNMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).MNMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).MNMinValDate);
                state.dataOutputProcessor->EnergyMeters(Meter).YRValue += state.dataOutputProcessor->EnergyMeters(Meter).TSValue;
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).YRMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).YRMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).YRMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).YRMinValDate);
                state.dataOutputProcessor->EnergyMeters(Meter).SMValue += state.dataOutputProcessor->EnergyMeters(Meter).TSValue;
                SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                          TimeStamp,
                          state.dataOutputProcessor->EnergyMeters(Meter).SMMaxVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).SMMaxValDate,
                          state.dataOutputProcessor->EnergyMeters(Meter).SMMinVal,
                          state.dataOutputProcessor->EnergyMeters(Meter).SMMinValDate);
                if (state.dataOutputProcessor->isFinalYear) {
                    state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMValue += state.dataOutputProcessor->EnergyMeters(Meter).TSValue;
                    SetMinMax(state.dataOutputProcessor->EnergyMeters(Meter).TSValue,
                              TimeStamp,
                              state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMaxVal,
                              state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMaxValDate,
                              state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMinVal,
                              state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMinValDate);
                }
            }
        }

        state.dataOutputProcessor->MeterValue = 0.0; // Ready for next update
    }

    void ResetAccumulationWhenWarmupComplete(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason Glazer
        //       DATE WRITTEN   June 2015
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Resets the accumulating meter values. Needed after warmup period is over to
        // reset the totals on meters so that they are not accumulated over the warmup period

        // METHODOLOGY EMPLOYED:
        // Cycle through the meters and reset all accumulating values

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Meter; // Loop Control
        int Loop;  // Loop Variable

        for (Meter = 1; Meter <= state.dataOutputProcessor->NumEnergyMeters; ++Meter) {
            state.dataOutputProcessor->EnergyMeters(Meter).HRValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(Meter).HRMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).HRMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(Meter).HRMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).HRMinValDate = 0;

            state.dataOutputProcessor->EnergyMeters(Meter).DYValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(Meter).DYMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).DYMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(Meter).DYMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).DYMinValDate = 0;

            state.dataOutputProcessor->EnergyMeters(Meter).MNValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(Meter).MNMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).MNMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(Meter).MNMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).MNMinValDate = 0;

            state.dataOutputProcessor->EnergyMeters(Meter).YRValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(Meter).YRMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).YRMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(Meter).YRMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).YRMinValDate = 0;

            state.dataOutputProcessor->EnergyMeters(Meter).SMValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(Meter).SMMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).SMMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(Meter).SMMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).SMMinValDate = 0;

            state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMValue = 0.0;
            state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMaxVal = MaxSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMaxValDate = 0;
            state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMinVal = MinSetValue;
            state.dataOutputProcessor->EnergyMeters(Meter).FinYrSMMinValDate = 0;
        }

        for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
            auto &rVar(state.dataOutputProcessor->RVariableTypes(Loop).VarPtr);
            if (rVar.frequency == ReportingFrequency::Monthly || rVar.frequency == ReportingFrequency::Yearly ||
                rVar.frequency == ReportingFrequency::Simulation) {
                rVar.StoreValue = 0.0;
                rVar.NumStored = 0;
            }
        }

        for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
            auto &iVar(state.dataOutputProcessor->IVariableTypes(Loop).VarPtr);
            if (iVar.frequency == ReportingFrequency::Monthly || iVar.frequency == ReportingFrequency::Yearly ||
                iVar.frequency == ReportingFrequency::Simulation) {
                iVar.StoreValue = 0;
                iVar.NumStored = 0;
            }
        }
    }

    void SetMinMax(Real64 const TestValue, // Candidate new value
                   int const TimeStamp,    // TimeStamp to be stored if applicable
                   Real64 &CurMaxValue,    // Current Maximum Value
                   int &CurMaxValDate,     // Current Maximum Value Date Stamp
                   Real64 &CurMinValue,    // Current Minimum Value
                   int &CurMinValDate      // Current Minimum Value Date Stamp
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine looks at the current value, comparing against the current max and
        // min for this meter/variable and resets along with a timestamp if applicable.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        if (TestValue > CurMaxValue) {
            CurMaxValue = TestValue;
            CurMaxValDate = TimeStamp;
        }
        if (TestValue < CurMinValue) {
            CurMinValue = TestValue;
            CurMinValDate = TimeStamp;
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop; // Loop Control
        bool PrintTimeStamp;
        int CurDayType;
        static Real64 rDummy1(0.0);
        static Real64 rDummy2(0.0);
        static int iDummy1(0);
        static int iDummy2(0);

        if (!ResultsFramework::resultsFramework->TSMeters.rDataFrameEnabled()) {
            ResultsFramework::resultsFramework->initializeMeters(state.dataOutputProcessor->EnergyMeters, ReportingFrequency::TimeStep);
        }

        PrintTimeStamp = true;
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumEnergyMeters; ++Loop) {
            state.dataOutputProcessor->EnergyMeters(Loop).CurTSValue = state.dataOutputProcessor->EnergyMeters(Loop).TSValue;
            if (!state.dataOutputProcessor->EnergyMeters(Loop).RptTS && !state.dataOutputProcessor->EnergyMeters(Loop).RptAccTS) continue;
            if (PrintTimeStamp) {
                CurDayType = state.dataEnvrn->DayOfWeek;
                if (state.dataEnvrn->HolidayIndex > 0) {
                    CurDayType = 7 + state.dataEnvrn->HolidayIndex;
                }
                WriteTimeStampFormatData(state,
                                         state.files.mtr,
                                         ReportingFrequency::EachCall,
                                         state.dataOutputProcessor->TimeStepStampReportNbr,
                                         state.dataOutputProcessor->TimeStepStampReportChr,
                                         state.dataGlobal->DayOfSimChr,
                                         PrintTimeStamp && PrintTimeStampToSQL,
                                         state.dataEnvrn->Month,
                                         state.dataEnvrn->DayOfMonth,
                                         state.dataGlobal->HourOfDay,
                                         EndMinute,
                                         StartMinute,
                                         state.dataEnvrn->DSTIndicator,
                                         DayTypes(CurDayType));
                if (ResultsFramework::resultsFramework->TSMeters.rDataFrameEnabled()) {
                    ResultsFramework::resultsFramework->TSMeters.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, EndMinute);
                }
                PrintTimeStamp = false;
                PrintTimeStampToSQL = false;
            }

            if (PrintESOTimeStamp && !state.dataOutputProcessor->EnergyMeters(Loop).RptTSFO && !state.dataOutputProcessor->EnergyMeters(Loop).RptAccTSFO) {
                CurDayType = state.dataEnvrn->DayOfWeek;
                if (state.dataEnvrn->HolidayIndex > 0) {
                    CurDayType = 7 + state.dataEnvrn->HolidayIndex;
                }
                WriteTimeStampFormatData(state,
                                         state.files.eso,
                                         ReportingFrequency::EachCall,
                                         state.dataOutputProcessor->TimeStepStampReportNbr,
                                         state.dataOutputProcessor->TimeStepStampReportChr,
                                         state.dataGlobal->DayOfSimChr,
                                         PrintTimeStamp && PrintESOTimeStamp && PrintTimeStampToSQL,
                                         state.dataEnvrn->Month,
                                         state.dataEnvrn->DayOfMonth,
                                         state.dataGlobal->HourOfDay,
                                         EndMinute,
                                         StartMinute,
                                         state.dataEnvrn->DSTIndicator,
                                         DayTypes(CurDayType));
                PrintESOTimeStamp = false;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptTS) {
                WriteReportMeterData(state,
                                     state.dataOutputProcessor->EnergyMeters(Loop).TSRptNum,
                                     state.dataOutputProcessor->EnergyMeters(Loop).TSRptNumChr,
                                     state.dataOutputProcessor->EnergyMeters(Loop).TSValue,
                                     ReportingFrequency::TimeStep,
                                     rDummy1,
                                     iDummy1,
                                     rDummy2,
                                     iDummy2,
                                     state.dataOutputProcessor->EnergyMeters(Loop).RptTSFO);
                ResultsFramework::resultsFramework->TSMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).TSRptNum, state.dataOutputProcessor->EnergyMeters(Loop).TSValue);
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptAccTS) {
                WriteCumulativeReportMeterData(state,
                                               state.dataOutputProcessor->EnergyMeters(Loop).TSAccRptNum,
                                               fmt::to_string(state.dataOutputProcessor->EnergyMeters(Loop).TSAccRptNum),
                                               state.dataOutputProcessor->EnergyMeters(Loop).SMValue,
                                               state.dataOutputProcessor->EnergyMeters(Loop).RptAccTSFO);
                ResultsFramework::resultsFramework->TSMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).TSAccRptNum, state.dataOutputProcessor->EnergyMeters(Loop).SMValue);
            }
        }

        if (state.dataOutputProcessor->NumEnergyMeters > 0) {
            for (auto &e : state.dataOutputProcessor->EnergyMeters)
                e.TSValue = 0.0;
        }
    }

    void ReportHRMeters(EnergyPlusData &state, bool PrintTimeStampToSQL // Print Time Stamp to SQL file
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports on the meters that have been requested for
        // reporting on each hour.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop; // Loop Control
        bool PrintTimeStamp;
        int CurDayType;
        static Real64 rDummy1(0.0);
        static Real64 rDummy2(0.0);
        static int iDummy1(0);
        static int iDummy2(0);

        if (!ResultsFramework::resultsFramework->HRMeters.rDataFrameEnabled()) {
            ResultsFramework::resultsFramework->initializeMeters(state.dataOutputProcessor->EnergyMeters, ReportingFrequency::Hourly);
        }

        PrintTimeStamp = true;
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumEnergyMeters; ++Loop) {
            if (!state.dataOutputProcessor->EnergyMeters(Loop).RptHR && !state.dataOutputProcessor->EnergyMeters(Loop).RptAccHR) continue;
            if (PrintTimeStamp) {
                CurDayType = state.dataEnvrn->DayOfWeek;
                if (state.dataEnvrn->HolidayIndex > 0) {
                    CurDayType = 7 + state.dataEnvrn->HolidayIndex;
                }
                WriteTimeStampFormatData(state,
                                         state.files.mtr,
                                         ReportingFrequency::Hourly,
                                         state.dataOutputProcessor->TimeStepStampReportNbr,
                                         state.dataOutputProcessor->TimeStepStampReportChr,
                                         state.dataGlobal->DayOfSimChr,
                                         PrintTimeStamp && PrintTimeStampToSQL,
                                         state.dataEnvrn->Month,
                                         state.dataEnvrn->DayOfMonth,
                                         state.dataGlobal->HourOfDay,
                                         _,
                                         _,
                                         state.dataEnvrn->DSTIndicator,
                                         DayTypes(CurDayType));
                if (ResultsFramework::resultsFramework->HRMeters.rDataFrameEnabled()) {
                    ResultsFramework::resultsFramework->HRMeters.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
                }
                PrintTimeStamp = false;
                PrintTimeStampToSQL = false;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptHR) {
                WriteReportMeterData(state,
                                     state.dataOutputProcessor->EnergyMeters(Loop).HRRptNum,
                                     state.dataOutputProcessor->EnergyMeters(Loop).HRRptNumChr,
                                     state.dataOutputProcessor->EnergyMeters(Loop).HRValue,
                                     ReportingFrequency::Hourly,
                                     rDummy1,
                                     iDummy1,
                                     rDummy2,
                                     iDummy2,
                                     state.dataOutputProcessor->EnergyMeters(Loop).RptHRFO); // EnergyMeters(Loop)%HRMinVal, EnergyMeters(Loop)%HRMinValDate, & |
                                                                  // EnergyMeters(Loop)%HRMaxVal, EnergyMeters(Loop)%HRMaxValDate, &
                ResultsFramework::resultsFramework->HRMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).HRRptNum, state.dataOutputProcessor->EnergyMeters(Loop).HRValue);
                state.dataOutputProcessor->EnergyMeters(Loop).HRValue = 0.0;
                state.dataOutputProcessor->EnergyMeters(Loop).HRMinVal = MinSetValue;
                state.dataOutputProcessor->EnergyMeters(Loop).HRMaxVal = MaxSetValue;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptAccHR) {
                WriteCumulativeReportMeterData(state,
                                               state.dataOutputProcessor->EnergyMeters(Loop).HRAccRptNum,
                                               fmt::to_string(state.dataOutputProcessor->EnergyMeters(Loop).HRAccRptNum),
                                               state.dataOutputProcessor->EnergyMeters(Loop).SMValue,
                                               state.dataOutputProcessor->EnergyMeters(Loop).RptAccHRFO);
                ResultsFramework::resultsFramework->HRMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).HRAccRptNum, state.dataOutputProcessor->EnergyMeters(Loop).SMValue);
            }
        }
    }

    void ReportDYMeters(EnergyPlusData &state, bool PrintTimeStampToSQL // Print Time Stamp to SQL file
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports on the meters that have been requested for
        // reporting on each day.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop; // Loop Control
        bool PrintTimeStamp;
        int CurDayType;

        if (!ResultsFramework::resultsFramework->DYMeters.rVariablesScanned()) {
            ResultsFramework::resultsFramework->initializeMeters(state.dataOutputProcessor->EnergyMeters, ReportingFrequency::Daily);
        }

        PrintTimeStamp = true;
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumEnergyMeters; ++Loop) {
            if (!state.dataOutputProcessor->EnergyMeters(Loop).RptDY && !state.dataOutputProcessor->EnergyMeters(Loop).RptAccDY) continue;
            if (PrintTimeStamp) {
                CurDayType = state.dataEnvrn->DayOfWeek;
                if (state.dataEnvrn->HolidayIndex > 0) {
                    CurDayType = 7 + state.dataEnvrn->HolidayIndex;
                }
                WriteTimeStampFormatData(state,
                                         state.files.mtr,
                                         ReportingFrequency::Daily,
                                         state.dataOutputProcessor->DailyStampReportNbr,
                                         state.dataOutputProcessor->DailyStampReportChr,
                                         state.dataGlobal->DayOfSimChr,
                                         PrintTimeStamp && PrintTimeStampToSQL,
                                         state.dataEnvrn->Month,
                                         state.dataEnvrn->DayOfMonth,
                                         _,
                                         _,
                                         _,
                                         state.dataEnvrn->DSTIndicator,
                                         DayTypes(CurDayType));
                if (ResultsFramework::resultsFramework->DYMeters.rDataFrameEnabled()) {
                    ResultsFramework::resultsFramework->DYMeters.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
                }
                PrintTimeStamp = false;
                PrintTimeStampToSQL = false;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptDY) {
                WriteReportMeterData(state,
                                     state.dataOutputProcessor->EnergyMeters(Loop).DYRptNum,
                                     state.dataOutputProcessor->EnergyMeters(Loop).DYRptNumChr,
                                     state.dataOutputProcessor->EnergyMeters(Loop).DYValue,
                                     ReportingFrequency::Daily,
                                     state.dataOutputProcessor->EnergyMeters(Loop).DYMinVal,
                                     state.dataOutputProcessor->EnergyMeters(Loop).DYMinValDate,
                                     state.dataOutputProcessor->EnergyMeters(Loop).DYMaxVal,
                                     state.dataOutputProcessor->EnergyMeters(Loop).DYMaxValDate,
                                     state.dataOutputProcessor->EnergyMeters(Loop).RptDYFO);
                ResultsFramework::resultsFramework->DYMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).DYRptNum, state.dataOutputProcessor->EnergyMeters(Loop).DYValue);
                state.dataOutputProcessor->EnergyMeters(Loop).DYValue = 0.0;
                state.dataOutputProcessor->EnergyMeters(Loop).DYMinVal = MinSetValue;
                state.dataOutputProcessor->EnergyMeters(Loop).DYMaxVal = MaxSetValue;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptAccDY) {
                WriteCumulativeReportMeterData(state,
                                               state.dataOutputProcessor->EnergyMeters(Loop).DYAccRptNum,
                                               fmt::to_string(state.dataOutputProcessor->EnergyMeters(Loop).DYAccRptNum),
                                               state.dataOutputProcessor->EnergyMeters(Loop).SMValue,
                                               state.dataOutputProcessor->EnergyMeters(Loop).RptAccDYFO);
                ResultsFramework::resultsFramework->DYMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).DYAccRptNum, state.dataOutputProcessor->EnergyMeters(Loop).SMValue);
            }
        }
    }

    void ReportMNMeters(EnergyPlusData &state, bool PrintTimeStampToSQL // Print Time Stamp to SQL file
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports on the meters that have been requested for
        // reporting on each month.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na


        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop; // Loop Control
        bool PrintTimeStamp;

        if (!ResultsFramework::resultsFramework->MNMeters.rVariablesScanned()) {
            ResultsFramework::resultsFramework->initializeMeters(state.dataOutputProcessor->EnergyMeters, ReportingFrequency::Monthly);
        }

        PrintTimeStamp = true;
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumEnergyMeters; ++Loop) {
            if (!state.dataOutputProcessor->EnergyMeters(Loop).RptMN && !state.dataOutputProcessor->EnergyMeters(Loop).RptAccMN) continue;
            if (PrintTimeStamp) {
                WriteTimeStampFormatData(state,
                                         state.files.mtr,
                                         ReportingFrequency::Monthly,
                                         state.dataOutputProcessor->MonthlyStampReportNbr,
                                         state.dataOutputProcessor->MonthlyStampReportChr,
                                         state.dataGlobal->DayOfSimChr,
                                         PrintTimeStamp && PrintTimeStampToSQL,
                                         state.dataEnvrn->Month);
                if (ResultsFramework::resultsFramework->MNMeters.rDataFrameEnabled()) {
                    ResultsFramework::resultsFramework->MNMeters.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
                }
                PrintTimeStamp = false;
                PrintTimeStampToSQL = false;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptMN) {
                WriteReportMeterData(state,
                                     state.dataOutputProcessor->EnergyMeters(Loop).MNRptNum,
                                     state.dataOutputProcessor->EnergyMeters(Loop).MNRptNumChr,
                                     state.dataOutputProcessor->EnergyMeters(Loop).MNValue,
                                     ReportingFrequency::Monthly,
                                     state.dataOutputProcessor->EnergyMeters(Loop).MNMinVal,
                                     state.dataOutputProcessor->EnergyMeters(Loop).MNMinValDate,
                                     state.dataOutputProcessor->EnergyMeters(Loop).MNMaxVal,
                                     state.dataOutputProcessor->EnergyMeters(Loop).MNMaxValDate,
                                     state.dataOutputProcessor->EnergyMeters(Loop).RptMNFO);
                ResultsFramework::resultsFramework->MNMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).MNRptNum, state.dataOutputProcessor->EnergyMeters(Loop).MNValue);
                state.dataOutputProcessor->EnergyMeters(Loop).MNValue = 0.0;
                state.dataOutputProcessor->EnergyMeters(Loop).MNMinVal = MinSetValue;
                state.dataOutputProcessor->EnergyMeters(Loop).MNMaxVal = MaxSetValue;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptAccMN) {
                WriteCumulativeReportMeterData(state,
                                               state.dataOutputProcessor->EnergyMeters(Loop).MNAccRptNum,
                                               fmt::to_string(state.dataOutputProcessor->EnergyMeters(Loop).MNAccRptNum),
                                               state.dataOutputProcessor->EnergyMeters(Loop).SMValue,
                                               state.dataOutputProcessor->EnergyMeters(Loop).RptAccMNFO);
                ResultsFramework::resultsFramework->MNMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).MNAccRptNum, state.dataOutputProcessor->EnergyMeters(Loop).SMValue);
            }
        }
    }

    void ReportYRMeters(EnergyPlusData &state, bool PrintTimeStampToSQL)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jason DeGraw
        //       DATE WRITTEN   January 2018
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports on the meters that have been requested for
        // reporting on each year.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop; // Loop Control
        bool PrintTimeStamp;

        if (!ResultsFramework::resultsFramework->YRMeters.rVariablesScanned()) {
            ResultsFramework::resultsFramework->initializeMeters(state.dataOutputProcessor->EnergyMeters, ReportingFrequency::Yearly);
        }

        PrintTimeStamp = true;
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumEnergyMeters; ++Loop) {
            if (!state.dataOutputProcessor->EnergyMeters(Loop).RptYR && !state.dataOutputProcessor->EnergyMeters(Loop).RptAccYR) continue;
            if (PrintTimeStamp) {
                WriteYearlyTimeStamp(
                    state, state.files.mtr, state.dataOutputProcessor->YearlyStampReportChr, state.dataGlobal->CalendarYearChr, PrintTimeStamp && PrintTimeStampToSQL);
                if (ResultsFramework::resultsFramework->YRMeters.rDataFrameEnabled()) {
                    ResultsFramework::resultsFramework->YRMeters.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
                }
                PrintTimeStamp = false;
                PrintTimeStampToSQL = false;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptYR) {
                WriteReportMeterData(state,
                                     state.dataOutputProcessor->EnergyMeters(Loop).YRRptNum,
                                     state.dataOutputProcessor->EnergyMeters(Loop).YRRptNumChr,
                                     state.dataOutputProcessor->EnergyMeters(Loop).YRValue,
                                     ReportingFrequency::Yearly,
                                     state.dataOutputProcessor->EnergyMeters(Loop).YRMinVal,
                                     state.dataOutputProcessor->EnergyMeters(Loop).YRMinValDate,
                                     state.dataOutputProcessor->EnergyMeters(Loop).YRMaxVal,
                                     state.dataOutputProcessor->EnergyMeters(Loop).YRMaxValDate,
                                     state.dataOutputProcessor->EnergyMeters(Loop).RptYRFO);
                ResultsFramework::resultsFramework->YRMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).YRRptNum, state.dataOutputProcessor->EnergyMeters(Loop).YRValue);
                state.dataOutputProcessor->EnergyMeters(Loop).YRValue = 0.0;
                state.dataOutputProcessor->EnergyMeters(Loop).YRMinVal = MinSetValue;
                state.dataOutputProcessor->EnergyMeters(Loop).YRMaxVal = MaxSetValue;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptAccYR) {
                WriteCumulativeReportMeterData(state,
                                               state.dataOutputProcessor->EnergyMeters(Loop).YRAccRptNum,
                                               fmt::to_string(state.dataOutputProcessor->EnergyMeters(Loop).YRAccRptNum),
                                               state.dataOutputProcessor->EnergyMeters(Loop).YRValue,
                                               state.dataOutputProcessor->EnergyMeters(Loop).RptAccYRFO);
                ResultsFramework::resultsFramework->YRMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).YRAccRptNum, state.dataOutputProcessor->EnergyMeters(Loop).SMValue);
            }
        }
    }

    void ReportSMMeters(EnergyPlusData &state, bool PrintTimeStampToSQL // Print Time Stamp to SQL file
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports on the meters that have been requested for
        // reporting on each environment/run period.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        // using namespace OutputReportPredefined;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop; // Loop Control
        bool PrintTimeStamp;

        if (!ResultsFramework::resultsFramework->SMMeters.rVariablesScanned()) {
            ResultsFramework::resultsFramework->initializeMeters(state.dataOutputProcessor->EnergyMeters, ReportingFrequency::Simulation);
        }

        PrintTimeStamp = true;
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumEnergyMeters; ++Loop) {
            state.dataOutputProcessor->EnergyMeters(Loop).LastSMValue = state.dataOutputProcessor->EnergyMeters(Loop).SMValue;
            state.dataOutputProcessor->EnergyMeters(Loop).LastSMMinVal = state.dataOutputProcessor->EnergyMeters(Loop).SMMinVal;
            state.dataOutputProcessor->EnergyMeters(Loop).LastSMMinValDate = state.dataOutputProcessor->EnergyMeters(Loop).SMMinValDate;
            state.dataOutputProcessor->EnergyMeters(Loop).LastSMMaxVal = state.dataOutputProcessor->EnergyMeters(Loop).SMMaxVal;
            state.dataOutputProcessor->EnergyMeters(Loop).LastSMMaxValDate = state.dataOutputProcessor->EnergyMeters(Loop).SMMaxValDate;
            if (!state.dataOutputProcessor->EnergyMeters(Loop).RptSM && !state.dataOutputProcessor->EnergyMeters(Loop).RptAccSM) continue;
            if (PrintTimeStamp) {
                WriteTimeStampFormatData(state,
                                         state.files.mtr,
                                         ReportingFrequency::Simulation,
                                         state.dataOutputProcessor->RunPeriodStampReportNbr,
                                         state.dataOutputProcessor->RunPeriodStampReportChr,
                                         state.dataGlobal->DayOfSimChr,
                                         PrintTimeStamp && PrintTimeStampToSQL);
                if (ResultsFramework::resultsFramework->SMMeters.rDataFrameEnabled()) {
                    ResultsFramework::resultsFramework->SMMeters.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
                }
                PrintTimeStamp = false;
                PrintTimeStampToSQL = false;
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptSM) {
                WriteReportMeterData(state,
                                     state.dataOutputProcessor->EnergyMeters(Loop).SMRptNum,
                                     state.dataOutputProcessor->EnergyMeters(Loop).SMRptNumChr,
                                     state.dataOutputProcessor->EnergyMeters(Loop).SMValue,
                                     ReportingFrequency::Simulation,
                                     state.dataOutputProcessor->EnergyMeters(Loop).SMMinVal,
                                     state.dataOutputProcessor->EnergyMeters(Loop).SMMinValDate,
                                     state.dataOutputProcessor->EnergyMeters(Loop).SMMaxVal,
                                     state.dataOutputProcessor->EnergyMeters(Loop).SMMaxValDate,
                                     state.dataOutputProcessor->EnergyMeters(Loop).RptSMFO);
                ResultsFramework::resultsFramework->SMMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).SMRptNum, state.dataOutputProcessor->EnergyMeters(Loop).SMValue);
            }

            if (state.dataOutputProcessor->EnergyMeters(Loop).RptAccSM) {
                WriteCumulativeReportMeterData(state,
                                               state.dataOutputProcessor->EnergyMeters(Loop).SMAccRptNum,
                                               fmt::to_string(state.dataOutputProcessor->EnergyMeters(Loop).SMAccRptNum),
                                               state.dataOutputProcessor->EnergyMeters(Loop).SMValue,
                                               state.dataOutputProcessor->EnergyMeters(Loop).RptAccSMFO);
                ResultsFramework::resultsFramework->SMMeters.pushVariableValue(state.dataOutputProcessor->EnergyMeters(Loop).SMAccRptNum, state.dataOutputProcessor->EnergyMeters(Loop).SMValue);
            }
        }

        if (state.dataOutputProcessor->NumEnergyMeters > 0) {
            for (auto &e : state.dataOutputProcessor->EnergyMeters) {
                e.SMValue = 0.0;
                e.SMMinVal = MinSetValue;
                e.SMMaxVal = MaxSetValue;
            }
        }
    }

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

        // Using/Aliasing
        using namespace OutputReportPredefined;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop; // Loop Control

        for (Loop = 1; Loop <= state.dataOutputProcessor->NumEnergyMeters; ++Loop) {
            int const RT_forIPUnits(state.dataOutputProcessor->EnergyMeters(Loop).RT_forIPUnits);
            if (RT_forIPUnits == RT_IPUnits_Electricity) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMelecannual, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMValue * DataGlobalConstants::convertJtoGJ);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMelecminvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMelecminvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinValDate));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMelecmaxvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMelecmaxvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxValDate));
            } else if (RT_forIPUnits == RT_IPUnits_Gas) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMgasannual, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMValue * DataGlobalConstants::convertJtoGJ);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMgasminvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMgasminvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinValDate));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMgasmaxvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMgasmaxvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxValDate));
            } else if (RT_forIPUnits == RT_IPUnits_Cooling) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMcoolannual, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMValue * DataGlobalConstants::convertJtoGJ);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMcoolminvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMcoolminvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinValDate));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMcoolmaxvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMcoolmaxvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxValDate));
            } else if (RT_forIPUnits == RT_IPUnits_Water) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMwaterannual, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMValue);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMwaterminvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMwaterminvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinValDate));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMwatermaxvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMwatermaxvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxValDate));
            } else if (RT_forIPUnits == RT_IPUnits_OtherKG) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherKGannual, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMValue);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherKGminvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinVal / state.dataGlobal->TimeStepZoneSec, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherKGminvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinValDate));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherKGmaxvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxVal / state.dataGlobal->TimeStepZoneSec, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherKGmaxvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxValDate));
            } else if (RT_forIPUnits == RT_IPUnits_OtherM3) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherM3annual, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMValue, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherM3minvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinVal / state.dataGlobal->TimeStepZoneSec, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherM3minvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinValDate));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherM3maxvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxVal / state.dataGlobal->TimeStepZoneSec, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherM3maxvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxValDate));
            } else if (RT_forIPUnits == RT_IPUnits_OtherL) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherLannual, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMValue, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherLminvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinVal / state.dataGlobal->TimeStepZoneSec, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherLminvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinValDate));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherLmaxvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxVal / state.dataGlobal->TimeStepZoneSec, 3);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherLmaxvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxValDate));
            } else {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherJannual, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMValue * DataGlobalConstants::convertJtoGJ);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherJminvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherJminvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMinValDate));
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherJmaxvalue, state.dataOutputProcessor->EnergyMeters(Loop).Name, state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxVal / state.dataGlobal->TimeStepZoneSec);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEMotherJmaxvaluetime, state.dataOutputProcessor->EnergyMeters(Loop).Name, DateToStringWithMonth(state.dataOutputProcessor->EnergyMeters(Loop).FinYrSMMaxValDate));
            }
        }
    }

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

        static constexpr auto DateFmt("{:02}-{:3}-{:02}:{:02}");

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

        const std::string StringOut = format(DateFmt, Day, monthName, Hour, Minute);
        return StringOut;
    }

    void ReportMeterDetails(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Writes the meter details report.  This shows which variables are on
        // meters as well as the meter contents.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        for (int VarMeter = 1; VarMeter <= state.dataOutputProcessor->NumVarMeterArrays; ++VarMeter) {

            const std::string mtrUnitString = unitEnumToStringBrackets(state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).units);

            std::string Multipliers = "";
            const auto ZoneMult = state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarPtr.ZoneMult;
            const auto ZoneListMult = state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarPtr.ZoneListMult;

            if (ZoneMult > 1 || ZoneListMult > 1) {
                Multipliers = format(" * {}  (Zone Multiplier = {}, Zone List Multiplier = {})", ZoneMult * ZoneListMult, ZoneMult, ZoneListMult);
            }

            print(state.files.mtd, "\n Meters for {},{}{}{}\n", state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarPtr.ReportIDChr,
                       state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarName, mtrUnitString, Multipliers);

            for (int I = 1; I <= state.dataOutputProcessor->VarMeterArrays(VarMeter).NumOnMeters; ++I) {
                print(state.files.mtd, "  OnMeter={}{}\n", state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->VarMeterArrays(VarMeter).OnMeters(I)).Name, mtrUnitString);
            }

            for (int I = 1; I <= state.dataOutputProcessor->VarMeterArrays(VarMeter).NumOnCustomMeters; ++I) {
                print(state.files.mtd, "  OnCustomMeter={}{}\n", state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->VarMeterArrays(VarMeter).OnCustomMeters(I)).Name, mtrUnitString);
            }
        }

        for (int Meter = 1; Meter <= state.dataOutputProcessor->NumEnergyMeters; ++Meter) {
            print(state.files.mtd, "\n For Meter={}{}", state.dataOutputProcessor->EnergyMeters(Meter).Name, unitEnumToStringBrackets(state.dataOutputProcessor->EnergyMeters(Meter).Units));
            if (state.dataOutputProcessor->EnergyMeters(Meter).ResourceType != "") {
                print(state.files.mtd, ", ResourceType={}", state.dataOutputProcessor->EnergyMeters(Meter).ResourceType);
            }
            if (state.dataOutputProcessor->EnergyMeters(Meter).EndUse != "") {
                print(state.files.mtd, ", EndUse={}", state.dataOutputProcessor->EnergyMeters(Meter).EndUse);
            }
            if (state.dataOutputProcessor->EnergyMeters(Meter).Group != "") {
                print(state.files.mtd, ", Group={}", state.dataOutputProcessor->EnergyMeters(Meter).Group);
            }
            print(state.files.mtd, ", contents are:\n");

            bool CustDecWritten = false;

            for (int VarMeter = 1; VarMeter <= state.dataOutputProcessor->NumVarMeterArrays; ++VarMeter) {
                if (state.dataOutputProcessor->EnergyMeters(Meter).TypeOfMeter == MeterType_Normal) {
                    if (any_eq(state.dataOutputProcessor->VarMeterArrays(VarMeter).OnMeters, Meter)) {
                        for (int VarMeter1 = 1; VarMeter1 <= state.dataOutputProcessor->VarMeterArrays(VarMeter).NumOnMeters; ++VarMeter1) {
                            if (state.dataOutputProcessor->VarMeterArrays(VarMeter).OnMeters(VarMeter1) != Meter) continue;

                            std::string Multipliers = "";
                            const auto ZoneMult = state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarPtr.ZoneMult;
                            const auto ZoneListMult = state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarPtr.ZoneListMult;

                            if (ZoneMult > 1 || ZoneListMult > 1) {
                                Multipliers = format(
                                    " * {}  (Zone Multiplier = {}, Zone List Multiplier = {})", ZoneMult * ZoneListMult, ZoneMult, ZoneListMult);
                            }

                            print(state.files.mtd, "  {}{}\n", state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarName, Multipliers);
                        }
                    }
                }
                if (state.dataOutputProcessor->EnergyMeters(Meter).TypeOfMeter != MeterType_Normal) {
                    if (state.dataOutputProcessor->VarMeterArrays(VarMeter).NumOnCustomMeters > 0) {
                        if (any_eq(state.dataOutputProcessor->VarMeterArrays(VarMeter).OnCustomMeters, Meter)) {
                            if (!CustDecWritten && state.dataOutputProcessor->EnergyMeters(Meter).TypeOfMeter == MeterType_CustomDec) {
                                print(state.files.mtd,  " Values for this meter will be Source Meter={}; but will be decremented by:\n", state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->EnergyMeters(Meter).SourceMeter).Name);
                                CustDecWritten = true;
                            }
                            for (int VarMeter1 = 1; VarMeter1 <= state.dataOutputProcessor->VarMeterArrays(VarMeter).NumOnCustomMeters; ++VarMeter1) {
                                if (state.dataOutputProcessor->VarMeterArrays(VarMeter).OnCustomMeters(VarMeter1) != Meter) continue;

                                std::string Multipliers;
                                const auto ZoneMult = state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarPtr.ZoneMult;
                                const auto ZoneListMult = state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarPtr.ZoneListMult;

                                if (ZoneMult > 1 || ZoneListMult > 1) {
                                    Multipliers = format(
                                        " * {}  (Zone Multiplier = {}, Zone List Multiplier = {})", ZoneMult * ZoneListMult, ZoneMult, ZoneListMult);
                                }

                                print(state.files.mtd, "  {}{}\n", state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(VarMeter).RepVariable).VarName, Multipliers);
                            }
                        }
                    }
                }
            }
        }
    }

    // *****************************************************************************
    // End of routines for Energy Meters implementation in EnergyPlus.
    // *****************************************************************************

    void AddEndUseSubcategory(EnergyPlusData &state,
                              [[maybe_unused]] std::string const &ResourceName,
                              std::string const &EndUseName,
                              std::string const &EndUseSubName)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   February 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages the list of subcategories for each end-use category.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int EndUseSubNum;
        int NumSubs;

        bool Found = false;
        for (size_t EndUseNum = 1; EndUseNum <= DataGlobalConstants::iEndUse.size(); ++EndUseNum) {
            if (UtilityRoutines::SameString(state.dataOutputProcessor->EndUseCategory(EndUseNum).Name, EndUseName)) {

                for (EndUseSubNum = 1; EndUseSubNum <= state.dataOutputProcessor->EndUseCategory(EndUseNum).NumSubcategories; ++EndUseSubNum) {
                    if (UtilityRoutines::SameString(state.dataOutputProcessor->EndUseCategory(EndUseNum).SubcategoryName(EndUseSubNum), EndUseSubName)) {
                        // Subcategory already exists, no further action required
                        Found = true;
                        break;
                    }
                }

                if (!Found) {
                    // Add the subcategory by reallocating the array
                    NumSubs = state.dataOutputProcessor->EndUseCategory(EndUseNum).NumSubcategories;
                    state.dataOutputProcessor->EndUseCategory(EndUseNum).SubcategoryName.redimension(NumSubs + 1);

                    state.dataOutputProcessor->EndUseCategory(EndUseNum).NumSubcategories = NumSubs + 1;
                    state.dataOutputProcessor->EndUseCategory(EndUseNum).SubcategoryName(NumSubs + 1) = EndUseSubName;

                    if (state.dataOutputProcessor->EndUseCategory(EndUseNum).NumSubcategories > state.dataOutputProcessor->MaxNumSubcategories) {
                        state.dataOutputProcessor->MaxNumSubcategories = state.dataOutputProcessor->EndUseCategory(EndUseNum).NumSubcategories;
                    }

                    Found = true;
                }
                break;
            }
        }

        if (!Found) {
            ShowSevereError(state, "Nonexistent end use passed to AddEndUseSubcategory=" + EndUseName);
        }
    }
    void WriteTimeStampFormatData(
        EnergyPlusData &state,
        InputOutputFile &outputFile,
        ReportingFrequency const reportingInterval, // See Module Parameter Definitions for ReportEach, ReportTimeStep, ReportHourly, etc.
        int const reportID,                         // The ID of the time stamp
        std::string const &reportIDString,          // The ID of the time stamp
        std::string const &DayOfSimChr,             // the number of days simulated so far
        bool writeToSQL,
        Optional_int_const Month,           // the month of the reporting interval
        Optional_int_const DayOfMonth,      // The day of the reporting interval
        Optional_int_const Hour,            // The hour of the reporting interval
        Optional<Real64 const> EndMinute,   // The last minute in the reporting interval
        Optional<Real64 const> StartMinute, // The starting minute of the reporting interval
        Optional_int_const DST,             // A flag indicating whether daylight savings time is observed
        Optional_string_const DayType       // The day tied for the data (e.g., Monday)
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        static int const N(100);
        static char stamp[N];
        assert(reportIDString.length() + DayOfSimChr.length() + (DayType.present() ? DayType().length() : 0u) + 26 <
               N); // Check will fit in stamp size

        if (!outputFile.good()) return;

        switch (reportingInterval) {
        case ReportingFrequency::EachCall:
        case ReportingFrequency::TimeStep:
            std::sprintf(stamp,
                         "%s,%s,%2d,%2d,%2d,%2d,%5.2f,%5.2f,%s",
                         reportIDString.c_str(),
                         DayOfSimChr.c_str(),
                         Month(),
                         DayOfMonth(),
                         DST(),
                         Hour(),
                         StartMinute(),
                         EndMinute(),
                         DayType().c_str());
            print(outputFile, "{}\n", stamp);
            if (writeToSQL && sqlite) {
                sqlite->createSQLiteTimeIndexRecord(static_cast<int>(reportingInterval),
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
        case ReportingFrequency::Hourly:
            std::sprintf(stamp,
                         "%s,%s,%2d,%2d,%2d,%2d,%5.2f,%5.2f,%s",
                         reportIDString.c_str(),
                         DayOfSimChr.c_str(),
                         Month(),
                         DayOfMonth(),
                         DST(),
                         Hour(),
                         0.0,
                         60.0,
                         DayType().c_str());
            print(outputFile, "{}\n", stamp);
            if (writeToSQL && sqlite) {
                sqlite->createSQLiteTimeIndexRecord(static_cast<int>(reportingInterval),
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
        case ReportingFrequency::Daily:
            std::sprintf(stamp, "%s,%s,%2d,%2d,%2d,%s", reportIDString.c_str(), DayOfSimChr.c_str(), Month(), DayOfMonth(), DST(), DayType().c_str());
            print(outputFile, "{}\n", stamp);
            if (writeToSQL && sqlite) {
                sqlite->createSQLiteTimeIndexRecord(static_cast<int>(reportingInterval),
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
        case ReportingFrequency::Monthly:
            std::sprintf(stamp, "%s,%s,%2d", reportIDString.c_str(), DayOfSimChr.c_str(), Month());
            print(outputFile, "{}\n", stamp);
            if (writeToSQL && sqlite) {
                sqlite->createSQLiteTimeIndexRecord(
                    static_cast<int>(reportingInterval), reportID, state.dataGlobal->DayOfSim, state.dataEnvrn->CurEnvirNum, state.dataGlobal->CalendarYear, Month);
            }
            break;
        case ReportingFrequency::Simulation:
            std::sprintf(stamp, "%s,%s", reportIDString.c_str(), DayOfSimChr.c_str());
            print(outputFile, "{}\n", stamp);
            if (writeToSQL && sqlite) {
                sqlite->createSQLiteTimeIndexRecord(
                    static_cast<int>(reportingInterval), reportID, state.dataGlobal->DayOfSim, state.dataEnvrn->CurEnvirNum, state.dataGlobal->CalendarYear);
            }
            break;
        default:
            if (sqlite) {
                std::string str(format("Illegal reportingInterval passed to WriteTimeStampFormatData: {}", static_cast<int>(reportingInterval)));
                sqlite->sqliteWriteMessage(str);
            }
            break;
        }
    }

    void WriteYearlyTimeStamp(EnergyPlusData &state,
                              InputOutputFile &outputFile,
                              std::string const &reportIDString,   // The ID of the time stamp
                              std::string const &yearOfSimChr,     // the year of the simulation
                              bool writeToSQL)
    {
        print(outputFile, "{},{}\n", reportIDString, yearOfSimChr);
        if (writeToSQL && sqlite) {
            sqlite->createYearlyTimeIndexRecord(state.dataGlobal->CalendarYear, state.dataEnvrn->CurEnvirNum);
        }
    }

    void WriteReportVariableDictionaryItem(EnergyPlusData &state,
                                           ReportingFrequency const reportingInterval, // The reporting interval (e.g., hourly, daily)
                                           StoreType const storeType,
                                           int const reportID,                       // The reporting ID for the data
                                           [[maybe_unused]] int const indexGroupKey, // The reporting group (e.g., Zone, Plant Loop, etc.)
                                           std::string const &indexGroup,            // The reporting group (e.g., Zone, Plant Loop, etc.)
                                           std::string const &reportIDChr,           // The reporting ID for the data
                                           std::string const &keyedValue,            // The key name for the data
                                           std::string const &variableName,          // The variable's actual name
                                           TimeStepType const timeStepType,
                                           OutputProcessor::Unit const &unitsForVar, // The variables units
                                           Optional_string_const customUnitName,
                                           Optional_string_const ScheduleName)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   August 2008
        //       MODIFIED       April 2011; Linda Lawrie
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine writes the ESO data dictionary information to the output files
        // and the SQL database

        // METHODOLOGY EMPLOYED:

        // REFERENCES:
        // na

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string FreqString;

        FreqString = frequencyNotice(storeType, reportingInterval);

        if (present(ScheduleName)) {
            FreqString += "," + ScheduleName;
        }

        std::string UnitsString;
        if (unitsForVar == OutputProcessor::Unit::customEMS && present(customUnitName)) {
            UnitsString = customUnitName;
        } else {
            UnitsString = unitEnumToString(unitsForVar);
        }

        const auto write = [&](InputOutputFile &file, const int interval) {
            if (file.good()) {
                print(file, "{},{},{},{} [{}]{}\n", reportIDChr, interval, keyedValue, variableName, UnitsString, FreqString);
            }
        };
        switch (reportingInterval) {
        case ReportingFrequency::EachCall:
        case ReportingFrequency::TimeStep:
            write(state.files.eso, 1);
            break;
        case ReportingFrequency::Hourly:
            state.dataOutputProcessor->TrackingHourlyVariables = true;
            write(state.files.eso, 1);
            break;
        case ReportingFrequency::Daily:
            state.dataOutputProcessor->TrackingDailyVariables = true;
            write(state.files.eso, 7);
            break;
        case ReportingFrequency::Monthly:
            state.dataOutputProcessor->TrackingMonthlyVariables = true;
            write(state.files.eso, 9);
            break;
        case ReportingFrequency::Simulation:
            state.dataOutputProcessor->TrackingRunPeriodVariables = true;
            write(state.files.eso, 11);
            break;
        case ReportingFrequency::Yearly:
            state.dataOutputProcessor->TrackingYearlyVariables = true;
            write(state.files.eso, 11);
            break;
            // No default available?
        }

        if (sqlite) {
            sqlite->createSQLiteReportDictionaryRecord(reportID,
                                                       static_cast<int>(storeType),
                                                       indexGroup,
                                                       keyedValue,
                                                       variableName,
                                                       static_cast<int>(timeStepType),
                                                       UnitsString,
                                                       static_cast<int>(reportingInterval),
                                                       false,
                                                       ScheduleName);
        }

        ResultsFramework::resultsFramework->addReportVariable(keyedValue, variableName, UnitsString, reportingInterval);

        // add to ResultsFramework for output variable list, need to check RVI/MVI later
    }

    void WriteMeterDictionaryItem(EnergyPlusData &state,
                                  ReportingFrequency const reportingInterval, // The reporting interval (e.g., hourly, daily)
                                  StoreType const storeType,
                                  int const reportID,                       // The reporting ID in for the variable
                                  [[maybe_unused]] int const indexGroupKey, // The reporting group for the variable
                                  std::string const &indexGroup,            // The reporting group for the variable
                                  std::string const &reportIDChr,           // The reporting ID in for the variable
                                  std::string const &meterName,             // The variable's meter name
                                  OutputProcessor::Unit const &unit,        // The variables units
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string UnitsString = unitEnumToString(unit);

        std::string const FreqString(frequencyNotice(storeType, reportingInterval));

        const auto print_meter = [&](EnergyPlusData &state, const int frequency) {
            const auto out = [&](InputOutputFile &of) {
                if (of.good()) {
                    if (cumulativeMeterFlag) {
                        static constexpr auto fmt{"{},{},Cumulative {} [{}]{}\n"};
                        const auto lenString = index(FreqString, '[');
                        print(of, fmt, reportIDChr, 1, meterName, UnitsString, FreqString.substr(0, lenString));
                    } else {
                        static constexpr auto fmt{"{},{},{} [{}]{}\n"};
                        print(of, fmt, reportIDChr, frequency, meterName, UnitsString, FreqString);
                    }
                }
            };

            out(state.files.mtr);
            if (!meterFileOnlyFlag) {
                out(state.files.eso);
            }
        };

        switch (reportingInterval) {
        case ReportingFrequency::EachCall:
        case ReportingFrequency::TimeStep:
        case ReportingFrequency::Hourly: // -1, 0, 1
            print_meter(state, 1);
            break;
        case ReportingFrequency::Daily: //  2
            print_meter(state, 7);
            break;
        case ReportingFrequency::Monthly: //  3
            print_meter(state, 9);
            break;
        case ReportingFrequency::Yearly: //  5
            print_meter(state, 11);
            break;
        case ReportingFrequency::Simulation: //  4
            print_meter(state, 11);
            break;
        }

        static std::string const keyedValueStringCum("Cumulative ");
        static std::string const keyedValueStringNon;
        std::string const &keyedValueString(cumulativeMeterFlag ? keyedValueStringCum : keyedValueStringNon);

        if (sqlite) {
            sqlite->createSQLiteReportDictionaryRecord(reportID,
                                                       static_cast<int>(storeType),
                                                       indexGroup,
                                                       keyedValueString,
                                                       meterName,
                                                       1,
                                                       UnitsString,
                                                       static_cast<int>(reportingInterval),
                                                       true);
        }

        ResultsFramework::resultsFramework->addReportMeter(meterName, UnitsString, reportingInterval);
        // add to ResultsFramework for output variable list, need to check RVI/MVI later
    }

    void WriteRealVariableOutput(EnergyPlusData &state,
                                 RealVariables &realVar,             // Real variable to write out
                                 ReportingFrequency const reportType // The report type or interval (e.g., hourly)
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        if (realVar.Report && realVar.frequency == reportType && realVar.Stored) {
            if (realVar.NumStored > 0.0) {
                WriteReportRealData(state,
                                    realVar.ReportID,
                                    realVar.ReportIDChr,
                                    realVar.StoreValue,
                                    realVar.storeType,
                                    realVar.NumStored,
                                    realVar.frequency,
                                    realVar.MinValue,
                                    realVar.minValueDate,
                                    realVar.MaxValue,
                                    realVar.maxValueDate);
                ++state.dataGlobal->StdOutputRecordCount;
            }

            realVar.StoreValue = 0.0;
            realVar.NumStored = 0.0;
            realVar.MinValue = MinSetValue;
            realVar.MaxValue = MaxSetValue;
            realVar.Stored = false;
        }
    }

    void WriteReportRealData(EnergyPlusData &state,
                             int const reportID,
                             std::string const &creportID,
                             Real64 const repValue,
                             StoreType const storeType,
                             Real64 const numOfItemsStored,
                             ReportingFrequency const reportingInterval,
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

        static char s[129];

        std::string NumberOut;   // Character for producing "number out"
        Real64 repVal(repValue); // The variable's value

        if (storeType == StoreType::Averaged) {
            repVal /= numOfItemsStored;
        }
        if (repVal == 0.0) {
            NumberOut = "0.0";
        } else {
            dtoa(repVal, s);
            NumberOut = std::string(s);
        }

        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
            //// The others (<= hourly) are handled inline with the code
            // add to daily TS data store
            if (reportingInterval == ReportingFrequency::Daily) {
                ResultsFramework::resultsFramework->RIDailyTSData.pushVariableValue(reportID, repVal);
            }
            // add to monthly TS data store
            if (reportingInterval == ReportingFrequency::Monthly) {
                ResultsFramework::resultsFramework->RIMonthlyTSData.pushVariableValue(reportID, repVal);
            }
            // add to run period TS data store
            if (reportingInterval == ReportingFrequency::Simulation) {
                ResultsFramework::resultsFramework->RIRunPeriodTSData.pushVariableValue(reportID, repVal);
            }
            // add to annual TS data store
            if (reportingInterval == ReportingFrequency::Yearly) {
                ResultsFramework::resultsFramework->RIYearlyTSData.pushVariableValue(reportID, repVal);
            }
        }

        if (sqlite) {
            sqlite->createSQLiteReportDataRecord(
                reportID, repVal, static_cast<int>(reportingInterval), minValue, minValueDate, MaxValue, maxValueDate);
        }

        if ((reportingInterval == ReportingFrequency::EachCall) || (reportingInterval == ReportingFrequency::TimeStep) ||
            (reportingInterval == ReportingFrequency::Hourly)) { // -1, 0, 1
            if (state.files.eso.good()) {
                print(state.files.eso, "{},{}\n", creportID, NumberOut);
            }

        } else { // if ( ( reportingInterval == ReportingFrequency::Daily ) || ( reportingInterval == ReportingFrequency::Monthly ) || (
                 // reportingInterval == ReportingFrequency::Simulation ) ) { //  2, 3, 4, 5
            std::string MaxOut; // Character for Max out string
            std::string MinOut; // Character for Min out string

            if (MaxValue == 0.0) {
                MaxOut = "0.0";
            } else {
                dtoa(MaxValue, s);
                MaxOut = std::string(s);
            }

            if (minValue == 0.0) {
                MinOut = "0.0";
            } else {
                dtoa(minValue, s);
                MinOut = std::string(s);
            }

            // Append the min and max strings with date information
            ProduceMinMaxString(MinOut, minValueDate, reportingInterval);
            ProduceMinMaxString(MaxOut, maxValueDate, reportingInterval);

            if (state.files.eso.good()) {
                print(state.files.eso, "{},{},{},{}\n", creportID, NumberOut, MinOut, MaxOut);
            }
        }
    }

    void WriteCumulativeReportMeterData(EnergyPlusData &state,
                                        int const reportID,           // The variable's report ID
                                        std::string const &creportID, // variable ID in characters
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

        static char s[129];
        std::string NumberOut; // Character for producing "number out"

        if (repValue == 0.0) {
            NumberOut = "0.0";
        } else {
            dtoa(repValue, s);
            NumberOut = std::string(s);
        }

        if (sqlite) {
            sqlite->createSQLiteReportDataRecord(reportID, repValue);
        }

        if (state.files.mtr.good()) print(state.files.mtr, "{},{}\n", creportID, NumberOut);
        ++state.dataGlobal->StdMeterRecordCount;

        if (!meterOnlyFlag) {
            if (state.files.eso.good()) print(state.files.eso, "{},{}\n", creportID, NumberOut);
            ++state.dataGlobal->StdOutputRecordCount;
        }
    }

    void WriteReportMeterData(EnergyPlusData &state,
                              int const reportID,                         // The variable's report ID
                              std::string const &creportID,               // variable ID in characters
                              Real64 const repValue,                      // The variable's value
                              ReportingFrequency const reportingInterval, // The variable's reporting interval (e.g., hourly)
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

        static char s[129];
        std::string NumberOut; // Character for producing "number out"

        if (repValue == 0.0) {
            NumberOut = "0.0";
        } else {
            dtoa(repValue, s);
            NumberOut = std::string(s);
        }

        if (sqlite) {
            sqlite->createSQLiteReportDataRecord(
                reportID, repValue, static_cast<int>(reportingInterval), minValue, minValueDate, MaxValue, maxValueDate, state.dataGlobal->MinutesPerTimeStep);
        }

        if ((reportingInterval == ReportingFrequency::EachCall) || (reportingInterval == ReportingFrequency::TimeStep) ||
            (reportingInterval == ReportingFrequency::Hourly)) { // -1, 0, 1
            if (state.files.mtr.good()) {
                print(state.files.mtr, "{},{}\n", creportID, NumberOut);
            }
            ++state.dataGlobal->StdMeterRecordCount;
            if (state.files.eso.good() && !meterOnlyFlag) {
                print(state.files.eso, "{},{}\n", creportID, NumberOut);
                ++state.dataGlobal->StdOutputRecordCount;
            }
        } else { // if ( ( reportingInterval == ReportDaily ) || ( reportingInterval == ReportMonthly ) || ( reportingInterval == ReportSim ) ) { //
                 // 2, 3, 4
            std::string MaxOut; // Character for Max out string
            std::string MinOut; // Character for Min out string

            if (MaxValue == 0.0) {
                MaxOut = "0.0";
            } else {
                dtoa(MaxValue, s);
                MaxOut = std::string(s);
            }

            if (minValue == 0.0) {
                MinOut = "0.0";
            } else {
                dtoa(minValue, s);
                MinOut = std::string(s);
            }

            // Append the min and max strings with date information
            ProduceMinMaxString(MinOut, minValueDate, reportingInterval);
            ProduceMinMaxString(MaxOut, maxValueDate, reportingInterval);

            if (state.files.mtr.good()) {
                print(state.files.mtr, "{},{},{},{}\n", creportID, NumberOut, MinOut, MaxOut);
            }

            ++state.dataGlobal->StdMeterRecordCount;
            if (state.files.eso.good() && !meterOnlyFlag) {
                print(state.files.eso, "{},{},{},{}\n", creportID, NumberOut, MinOut, MaxOut);
                ++state.dataGlobal->StdOutputRecordCount;
            }
        }
    }

    void WriteNumericData(EnergyPlusData &state,
                          int const reportID,           // The variable's reporting ID
                          std::string const &creportID, // variable ID in characters
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

        static char s[129];

        if (DataSystemVariables::UpdateDataDuringWarmupExternalInterface && !DataSystemVariables::ReportDuringWarmup) return;

        dtoa(repValue, s);

        if (sqlite) {
            sqlite->createSQLiteReportDataRecord(reportID, repValue);
        }

        if (state.files.eso.good()) {
            print(state.files.eso, "{},{}\n", creportID, s);
        }
    }

    void WriteNumericData(EnergyPlusData &state,
                          int const reportID,           // The variable's reporting ID
                          std::string const &creportID, // variable ID in characters
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

        static char s[129];

        i32toa(repValue, s);

        if (sqlite) {
            sqlite->createSQLiteReportDataRecord(reportID, repValue);
        }

        if (state.files.eso.good()) {
            print(state.files.eso, "{},{}\n", creportID, s);
        }
    }

    void WriteNumericData(EnergyPlusData &state,
                          int const reportID,           // The variable's reporting ID
                          std::string const &creportID, // variable ID in characters
                          int64_t const repValue        // The variable's value
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

        static char s[129];

        i64toa(repValue, s);

        if (sqlite) {
            sqlite->createSQLiteReportDataRecord(reportID, repValue);
        }

        if (state.files.eso.good()) {
            print(state.files.eso, "{},{}\n", creportID, s);
        }
    }

    void WriteIntegerVariableOutput(EnergyPlusData &state,
                                    IntegerVariables &intVar,           // Integer variable to write out
                                    ReportingFrequency const reportType // The report type (i.e., the reporting interval)
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataSystemVariables::ReportDuringWarmup;
        using DataSystemVariables::UpdateDataDuringWarmupExternalInterface;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        if (UpdateDataDuringWarmupExternalInterface && !ReportDuringWarmup) return;

        if (intVar.Report && intVar.frequency == reportType && intVar.Stored) {
            if (intVar.NumStored > 0.0) {
                WriteReportIntegerData(state,
                                       intVar.ReportID,
                                       intVar.ReportIDChr,
                                       intVar.StoreValue,
                                       intVar.storeType,
                                       intVar.NumStored,
                                       intVar.frequency,
                                       intVar.MinValue,
                                       intVar.minValueDate,
                                       intVar.MaxValue,
                                       intVar.maxValueDate);
                ++state.dataGlobal->StdOutputRecordCount;
            }

            intVar.StoreValue = 0.0;
            intVar.NumStored = 0.0;
            intVar.MinValue = IMinSetValue;
            intVar.MaxValue = IMaxSetValue;
            intVar.Stored = false;
        }
    }

    void WriteReportIntegerData(EnergyPlusData &state,
                                int const reportID,                         // The variable's reporting ID
                                std::string const &reportIDString,          // The variable's reporting ID (character)
                                Real64 const repValue,                      // The variable's value
                                StoreType const storeType,                  // Type of item (averaged or summed)
                                Real64 const numOfItemsStored,              // The number of items (hours or timesteps) of data stored
                                ReportingFrequency const reportingInterval, // The reporting interval (e.g., monthly)
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using General::strip_trailing_zeros;

        // Locals

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string NumberOut; // Character for producing "number out"
        std::string MaxOut;    // Character for Max out string
        std::string MinOut;    // Character for Min out string
        Real64 rmaxValue;
        Real64 rminValue;
        Real64 repVal; // The variable's value

        repVal = repValue;
        if (storeType == StoreType::Averaged) {
            repVal /= numOfItemsStored;
        }
        if (repValue == 0.0) {
            NumberOut = "0.0";
        } else {
            NumberOut = format("{:N}", repVal);
            strip_trailing_zeros(trim(NumberOut));
        }

        // Append the min and max strings with date information
        MinOut = fmt::to_string(minValue);
        MaxOut = fmt::to_string(MaxValue);
        ProduceMinMaxString(MinOut, minValueDate, reportingInterval);
        ProduceMinMaxString(MaxOut, maxValueDate, reportingInterval);

        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
            // add to daily TS data store
            if (reportingInterval == ReportingFrequency::Daily) {
                ResultsFramework::resultsFramework->RIDailyTSData.pushVariableValue(reportID, repVal);
            }
            // add to monthly TS data store
            if (reportingInterval == ReportingFrequency::Monthly) {
                ResultsFramework::resultsFramework->RIMonthlyTSData.pushVariableValue(reportID, repVal);
            }
            // add to run period TS data store
            if (reportingInterval == ReportingFrequency::Simulation) {
                ResultsFramework::resultsFramework->RIRunPeriodTSData.pushVariableValue(reportID, repVal);
            }
            // add to annual TS data store
            if (reportingInterval == ReportingFrequency::Yearly) {
                ResultsFramework::resultsFramework->RIYearlyTSData.pushVariableValue(reportID, repVal);
            }
        }

        rminValue = minValue;
        rmaxValue = MaxValue;
        if (sqlite) {
            sqlite->createSQLiteReportDataRecord(
                reportID, repVal, static_cast<int>(reportingInterval), rminValue, minValueDate, rmaxValue, maxValueDate);
        }

        if ((reportingInterval == ReportingFrequency::EachCall) || (reportingInterval == ReportingFrequency::TimeStep) ||
            (reportingInterval == ReportingFrequency::Hourly)) { // -1, 0, 1
            if (state.files.eso.good()) {
                print(state.files.eso, "{},{}\n", reportIDString, NumberOut);
            }
        } else { // if ( ( reportingInterval == ReportDaily ) || ( reportingInterval == ReportMonthly ) || ( reportingInterval == ReportSim ) ) { //
                 // 2, 3, 4
            if (state.files.eso.good()) {
                print(state.files.eso, "{},{},{},{}\n", reportIDString, NumberOut, MinOut, MaxOut);
            }
        }
    }

    int DetermineIndexGroupKeyFromMeterName(std::string const &meterName) // the meter name
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function attemps to guess determine how a meter variable should be
        // grouped.  It does this by parsing the meter name and then assigns a
        // indexGroupKey based on the name

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        int DetermineIndexGroupKeyFromMeterName;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        static int indexGroupKey(-1);

        // Facility indices are in the 100s
        if (has(meterName, "Electricity:Facility")) {
            indexGroupKey = 100;
        } else if (has(meterName, "NaturalGas:Facility")) {
            indexGroupKey = 101;
        } else if (has(meterName, "DistricHeating:Facility")) {
            indexGroupKey = 102;
        } else if (has(meterName, "DistricCooling:Facility")) {
            indexGroupKey = 103;
        } else if (has(meterName, "ElectricityNet:Facility")) {
            indexGroupKey = 104;

            // Building indices are in the 200s
        } else if (has(meterName, "Electricity:Building")) {
            indexGroupKey = 201;
        } else if (has(meterName, "NaturalGas:Building")) {
            indexGroupKey = 202;

            // HVAC indices are in the 300s
        } else if (has(meterName, "Electricity:HVAC")) {
            indexGroupKey = 301;

            // InteriorLights:Electricity:Zone indices are in the 500s
        } else if (has(meterName, "InteriorLights:Electricity:Zone")) {
            indexGroupKey = 501;

            // InteriorLights:Electricity indices are in the 400s
        } else if (has(meterName, "InteriorLights:Electricity")) {
            indexGroupKey = 401;

            // Unknown items have negative indices
        } else {
            indexGroupKey = -11;
        }

        DetermineIndexGroupKeyFromMeterName = indexGroupKey;

        return DetermineIndexGroupKeyFromMeterName;
    }

    std::string DetermineIndexGroupFromMeterGroup(MeterType const &meter) // the meter
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Greg Stark
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function attemps to determine how a meter variable should be
        // grouped.  It does this by parsing the meter group

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        std::string indexGroup;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        if (len(meter.Group) > 0) {
            indexGroup = meter.Group;
        } else {
            indexGroup = "Facility";
        }

        if (len(meter.ResourceType) > 0) {
            indexGroup += ":" + meter.ResourceType;
        }

        if (len(meter.EndUse) > 0) {
            indexGroup += ":" + meter.EndUse;
        }

        if (len(meter.EndUseSub) > 0) {
            indexGroup += ":" + meter.EndUseSub;
        }

        return indexGroup;
    }

    void SetInternalVariableValue(EnergyPlusData &state,
                                  int const varType,       // 1=integer, 2=real, 3=meter
                                  int const keyVarIndex,   // Array index
                                  Real64 const SetRealVal, // real value to set, if type is real or meter
                                  int const SetIntVal      // integer value to set if type is integer
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   August 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This is a simple set routine for output pointers
        // It is intended for special use to reinitializations those pointers used for EMS sensors

        // METHODOLOGY EMPLOYED:
        // given a variable type and variable index,
        // assign the pointers the values passed in.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        if (varType == 1) { // Integer
            *state.dataOutputProcessor->IVariableTypes(keyVarIndex).VarPtr.Which = SetIntVal;
        } else if (varType == 2) { // real
            *state.dataOutputProcessor->RVariableTypes(keyVarIndex).VarPtr.Which = SetRealVal;
        } else if (varType == 3) { // meter
            state.dataOutputProcessor->EnergyMeters(keyVarIndex).CurTSValue = SetRealVal;
        }
    }

    // returns the string corresponding to the OutputProcessor::Unit enum in brackets
    std::string unitEnumToStringBrackets(Unit const &unitIn)
    {
        // J.Glazer - August/September 2017
        return " [" + unitEnumToString(unitIn) + "]";
    }

    // returns the unit string for a DDVariableTypes item and custom string when customEMS is used
    std::string unitStringFromDDitem(EnergyPlusData &state, int const ddItemPtr // index provided for DDVariableTypes
    )
    {
        // J.Glazer - August/September 2017
        OutputProcessor::Unit ddUnit = state.dataOutputProcessor->DDVariableTypes(ddItemPtr).units;
        if (ddUnit != OutputProcessor::Unit::customEMS) {
            return unitEnumToStringBrackets(ddUnit);
        } else {
            return " [" + state.dataOutputProcessor->DDVariableTypes(ddItemPtr).unitNameCustomEMS + "]";
        }
    }

    // returns the string corresponding to the OutputProcessor::Unit enum
    std::string unitEnumToString(Unit const &unitIn)
    {
        // J.Glazer - August/September 2017
        switch (unitIn) {
        case OutputProcessor::Unit::J:
            return "J";
            break;
        case OutputProcessor::Unit::W:
            return "W";
            break;
        case OutputProcessor::Unit::C:
            return "C";
            break;
        case OutputProcessor::Unit::None:
            return "";
            break;
        case OutputProcessor::Unit::kg:
            return "kg";
            break;
        case OutputProcessor::Unit::W_m2:
            return "W/m2";
            break;
        case OutputProcessor::Unit::m3:
            return "m3";
            break;
        case OutputProcessor::Unit::hr:
            return "hr";
            break;
        case OutputProcessor::Unit::kg_s:
            return "kg/s";
            break;
        case OutputProcessor::Unit::deg:
            return "deg";
            break;
        case OutputProcessor::Unit::m3_s:
            return "m3/s";
            break;
        case OutputProcessor::Unit::W_m2K:
            return "W/m2-K";
            break;
        case OutputProcessor::Unit::kgWater_kgDryAir:
            return "kgWater/kgDryAir";
            break;
        case OutputProcessor::Unit::Perc:
            return "%";
            break;
        case OutputProcessor::Unit::m_s:
            return "m/s";
            break;
        case OutputProcessor::Unit::lux:
            return "lux";
            break;
        case OutputProcessor::Unit::kgWater_s:
            return "kgWater/s";
            break;
        case OutputProcessor::Unit::rad:
            return "rad";
            break;
        case OutputProcessor::Unit::Pa:
            return "Pa";
            break;
        case OutputProcessor::Unit::J_kg:
            return "J/kg";
            break;
        case OutputProcessor::Unit::m:
            return "m";
            break;
        case OutputProcessor::Unit::lum_W:
            return "lum/W";
            break;
        case OutputProcessor::Unit::kg_m3:
            return "kg/m3";
            break;
        case OutputProcessor::Unit::L:
            return "L";
            break;
        case OutputProcessor::Unit::ach:
            return "ach";
            break;
        case OutputProcessor::Unit::m2:
            return "m2";
            break;
        case OutputProcessor::Unit::deltaC:
            return "deltaC";
            break;
        case OutputProcessor::Unit::J_kgK:
            return "J/kg-K";
            break;
        case OutputProcessor::Unit::W_W:
            return "W/W";
            break;
        case OutputProcessor::Unit::clo:
            return "clo";
            break;
        case OutputProcessor::Unit::W_K:
            return "W/K";
            break;
        case OutputProcessor::Unit::K_W:
            return "K/W";
            break;
        case OutputProcessor::Unit::ppm:
            return "ppm";
            break;
        case OutputProcessor::Unit::kg_kg:
            return "kg/kg";
            break;
        case OutputProcessor::Unit::s:
            return "s";
            break;
        case OutputProcessor::Unit::cd_m2:
            return "cd/m2";
            break;
        case OutputProcessor::Unit::kmol_s:
            return "kmol/s";
            break;
        case OutputProcessor::Unit::K_m:
            return "K/m";
            break;
        case OutputProcessor::Unit::min:
            return "min";
            break;
        case OutputProcessor::Unit::J_kgWater:
            return "J/kgWater";
            break;
        case OutputProcessor::Unit::rev_min:
            return "rev/min";
            break;
        case OutputProcessor::Unit::kg_m2s:
            return "kg/m2-s";
            break;
        case OutputProcessor::Unit::J_m2:
            return "J/m2";
            break;
        case OutputProcessor::Unit::A:
            return "A";
            break;
        case OutputProcessor::Unit::V:
            return "V";
            break;
        case OutputProcessor::Unit::W_m2C:
            return "W/m2-C";
            break;
        case OutputProcessor::Unit::Ah:
            return "Ah";
            break;
        case OutputProcessor::Unit::Btu_h_W:
            return "Btu/h-W";
            break;
        default:
            return "unknown";
            break;
        }
    }

    // returns the OutputProcessor::Unit enum value when a string containing the units is provided without brackets
    OutputProcessor::Unit unitStringToEnum(std::string const &unitIn)
    {
        // J.Glazer - August/September 2017
        std::string unitUpper = UtilityRoutines::MakeUPPERCase(unitIn);
        if (unitUpper == "J") {
            return OutputProcessor::Unit::J;
        } else if (unitUpper == "DELTAC") {
            return OutputProcessor::Unit::deltaC;
        } else if (unitUpper == "") {
            return OutputProcessor::Unit::None;
        } else if (unitUpper == "W") {
            return OutputProcessor::Unit::W;
        } else if (unitUpper == "C") {
            return OutputProcessor::Unit::C;
        } else if (unitUpper == "KG/S") {
            return OutputProcessor::Unit::kg_s;
        } else if (unitUpper == "KGWATER/KGDRYAIR") {
            return OutputProcessor::Unit::kgWater_kgDryAir;
        } else if (unitUpper == "PPM") {
            return OutputProcessor::Unit::ppm;
        } else if (unitUpper == "PA") {
            return OutputProcessor::Unit::Pa;
        } else if (unitUpper == "M3/S") {
            return OutputProcessor::Unit::m3_s;
        } else if (unitUpper == "MIN") {
            return OutputProcessor::Unit::min;
        } else if (unitUpper == "M3") {
            return OutputProcessor::Unit::m3;
        } else if (unitUpper == "KG") {
            return OutputProcessor::Unit::kg;
        } else if (unitUpper == "ACH") {
            return OutputProcessor::Unit::ach;
        } else if (unitUpper == "W/W") {
            return OutputProcessor::Unit::W_W;
        } else if (unitUpper == "LUX") {
            return OutputProcessor::Unit::lux;
        } else if (unitUpper == "LUM/W") {
            return OutputProcessor::Unit::lum_W;
        } else if (unitUpper == "HR") {
            return OutputProcessor::Unit::hr;
        } else if (unitUpper == "CD/M2") {
            return OutputProcessor::Unit::cd_m2;
        } else if (unitUpper == "J/KGWATER") {
            return OutputProcessor::Unit::J_kgWater;
        } else if (unitUpper == "M/S") {
            return OutputProcessor::Unit::m_s;
        } else if (unitUpper == "W/M2") {
            return OutputProcessor::Unit::W_m2;
        } else if (unitUpper == "M") {
            return OutputProcessor::Unit::m;
        } else if (unitUpper == "AH") {
            return OutputProcessor::Unit::Ah;
        } else if (unitUpper == "A") {
            return OutputProcessor::Unit::A;
        } else if (unitUpper == "V") {
            return OutputProcessor::Unit::V;
        } else if (unitUpper == "KMOL/S") {
            return OutputProcessor::Unit::kmol_s;
        } else if (unitUpper == "KG/S") {
            return OutputProcessor::Unit::rev_min;
        } else if (unitUpper == "W/M2-K") {
            return OutputProcessor::Unit::W_m2K;
        } else if (unitUpper == "J/KG") {
            return OutputProcessor::Unit::J_kg;
        } else if (unitUpper == "KG/KG") {
            return OutputProcessor::Unit::kg_kg;
        } else if (unitUpper == "%") {
            return OutputProcessor::Unit::Perc;
        } else if (unitUpper == "DEG") {
            return OutputProcessor::Unit::deg;
        } else if (unitUpper == "S") {
            return OutputProcessor::Unit::s;
        } else if (unitUpper == "KG/M3") {
            return OutputProcessor::Unit::kg_m3;
        } else if (unitUpper == "KG/M2-S") {
            return OutputProcessor::Unit::kg_m2s;
        } else if (unitUpper == "J/KG-K") {
            return OutputProcessor::Unit::J_kgK;
        } else if (unitUpper == "L") {
            return OutputProcessor::Unit::L;
        } else if (unitUpper == "K/M") {
            return OutputProcessor::Unit::K_m;
        } else if (unitUpper == "M2") {
            return OutputProcessor::Unit::m2;
        } else if (unitUpper == "W/M2-C") {
            return OutputProcessor::Unit::W_m2C;
        } else if (unitUpper == "RAD") {
            return OutputProcessor::Unit::rad;
        } else if (unitUpper == "J/M2") {
            return OutputProcessor::Unit::J_m2;
        } else if (unitUpper == "CLO") {
            return OutputProcessor::Unit::clo;
        } else if (unitUpper == "W/K") {
            return OutputProcessor::Unit::W_K;
        } else if (unitUpper == "K/W") {
            return OutputProcessor::Unit::K_W;
        } else if (unitUpper == "KGWATER/S") {
            return OutputProcessor::Unit::kgWater_s;
        } else {
            return OutputProcessor::Unit::unknown;
        }
    }

} // namespace OutputProcessor

//==============================================================================================
// *****************************************************************************
// These routines are available outside the OutputProcessor Module (i.e. calling
// routines do not have to "USE OutputProcessor".  But each of these routines
// will use the OutputProcessor and take advantage that everything is PUBLIC
// within the OutputProcessor.
// *****************************************************************************

void SetupOutputVariable(EnergyPlusData &state,
                         std::string const &VariableName,           // String Name of variable (with units)
                         OutputProcessor::Unit const &VariableUnit, // Actual units corresponding to the actual variable
                         Real64 &ActualVariable,                    // Actual Variable, used to set up pointer
                         std::string const &TimeStepTypeKey,        // Zone, HeatBalance=1, HVAC, System, Plant=2
                         std::string const &VariableTypeKey,        // State, Average=1, NonState, Sum=2
                         std::string const &KeyedValue,             // Associated Key for this variable
                         Optional_string_const ReportFreq,          // Internal use -- causes reporting at this freqency
                         Optional_string_const ResourceTypeKey,     // Meter Resource Type (Electricity, Gas, etc)
                         Optional_string_const EndUseKey,           // Meter End Use Key (Lights, Heating, Cooling, etc)
                         Optional_string_const EndUseSubKey,        // Meter End Use Sub Key (General Lights, Task Lights, etc)
                         Optional_string_const GroupKey,            // Meter Super Group Key (Building, System, Plant)
                         Optional_string_const ZoneKey,             // Meter Zone Key (zone name)
                         Optional_int_const ZoneMult,               // Zone Multiplier, defaults to 1
                         Optional_int_const ZoneListMult,           // Zone List Multiplier, defaults to 1
                         Optional_int_const indexGroupKey,          // Group identifier for SQL output
                         Optional_string_const customUnitName       // the custom name for the units from EMS definition of units
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

    // Using/Aliasing
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CV;
    TimeStepType TimeStepType; // 1=TimeStepZone, 2=TimeStepSys
    StoreType VariableType;    // 1=Average, 2=Sum, 3=Min/Max
    int Loop;
    ReportingFrequency RepFreq(ReportingFrequency::Hourly);
    std::string ResourceType;       // Will hold value of ResourceTypeKey
    std::string EndUse;             // Will hold value of EndUseKey
    std::string EndUseSub;          // Will hold value of EndUseSubKey
    std::string Group;              // Will hold value of GroupKey
    std::string ZoneName;           // Will hold value of ZoneKey
    static bool ErrorsFound(false); // True if Errors Found
    int localIndexGroupKey;

    if (!state.dataOutputProcessor->OutputInitialized) InitializeOutput(state);

    // Variable name without units
    const std::string& VarName = VariableName;

    // Determine whether to Report or not
    CheckReportVariable(state, KeyedValue, VarName);

    if (state.dataOutputProcessor->NumExtraVars == 0) {
        state.dataOutputProcessor->NumExtraVars = 1;
        state.dataOutputProcessor->ReportList = -1;
    }

    // If ReportFreq present, overrides input
    if (present(ReportFreq)) {
        RepFreq = determineFrequency(state, ReportFreq);
        state.dataOutputProcessor->NumExtraVars = 1;
        state.dataOutputProcessor->ReportList = 0;
    }

    // DataOutputs::OutputVariablesForSimulation is case-insensitive
    bool const ThisOneOnTheList = DataOutputs::FindItemInVariableList(KeyedValue, VarName);
    bool OnMeter = false; // True if this variable is on a meter

    for (Loop = 1; Loop <= state.dataOutputProcessor->NumExtraVars; ++Loop) {

        if (Loop == 1) ++state.dataOutputProcessor->NumOfRVariable_Setup;

        if (Loop == 1) {
            OnMeter = false;
            if (present(ResourceTypeKey)) {
                ResourceType = ResourceTypeKey;
                OnMeter = true;
            } else {
                ResourceType = "";
            }
            if (present(EndUseKey)) {
                EndUse = EndUseKey;
                OnMeter = true;
            } else {
                EndUse = "";
            }
            if (present(EndUseSubKey)) {
                EndUseSub = EndUseSubKey;
                OnMeter = true;
            } else {
                 EndUseSub = "";
                 if (present(EndUseKey)) {
                    if (std::find(endUseCategoryNames.begin(), endUseCategoryNames.end(), UtilityRoutines::MakeUPPERCase(EndUseKey)) != endUseCategoryNames.end()) {
                        EndUseSub = "General";
                    }
                }
            }
            if (present(GroupKey)) {
                Group = GroupKey;
                OnMeter = true;
            } else {
                Group = "";
            }
            if (present(ZoneKey)) {
                ZoneName = ZoneKey;
                OnMeter = true;
            } else {
                ZoneName = "";
            }
        }

        TimeStepType = ValidateTimeStepType(state, TimeStepTypeKey, "SetupOutputVariable");
        VariableType = validateVariableType(state, VariableTypeKey);

        if (present(customUnitName)) {
            AddToOutputVariableList(state, VarName, TimeStepType, VariableType, VarType_Real, VariableUnit, customUnitName);
        } else {
            AddToOutputVariableList(state, VarName, TimeStepType, VariableType, VarType_Real, VariableUnit);
        }
        ++state.dataOutputProcessor->NumTotalRVariable;

        if (!OnMeter && !ThisOneOnTheList) continue;

        ++state.dataOutputProcessor->NumOfRVariable;
        if (Loop == 1 && VariableType == StoreType::Summed) {
            ++state.dataOutputProcessor->NumOfRVariable_Sum;
            if (present(ResourceTypeKey)) {
                if (!ResourceTypeKey().empty()) ++state.dataOutputProcessor->NumOfRVariable_Meter;
            }
        }
        if (state.dataOutputProcessor->NumOfRVariable > state.dataOutputProcessor->MaxRVariable) {
            ReallocateRVar(state);
        }
        CV = state.dataOutputProcessor->NumOfRVariable;
        auto &thisRvar = state.dataOutputProcessor->RVariableTypes(CV);
        thisRvar.timeStepType = TimeStepType;
        thisRvar.storeType = VariableType;
        thisRvar.VarName = KeyedValue + ':' + VarName;
        thisRvar.VarNameOnly = VarName;
        thisRvar.VarNameOnlyUC = UtilityRoutines::MakeUPPERCase(VarName);
        thisRvar.VarNameUC = UtilityRoutines::MakeUPPERCase(thisRvar.VarName);
        thisRvar.KeyNameOnlyUC = UtilityRoutines::MakeUPPERCase(KeyedValue);
        thisRvar.units = VariableUnit;
        if (VariableUnit == OutputProcessor::Unit::customEMS) {
            thisRvar.unitNameCustomEMS = customUnitName;
        }
        AssignReportNumber(state, state.dataOutputProcessor->CurrentReportNumber);
        const auto IDOut = fmt::to_string(state.dataOutputProcessor->CurrentReportNumber);
        thisRvar.ReportID = state.dataOutputProcessor->CurrentReportNumber;
        auto &thisVarPtr = thisRvar.VarPtr;
        thisVarPtr.Value = 0.0;
        thisVarPtr.TSValue = 0.0;
        thisVarPtr.StoreValue = 0.0;
        thisVarPtr.NumStored = 0.0;
        thisVarPtr.MaxValue = MaxSetValue;
        thisVarPtr.maxValueDate = 0;
        thisVarPtr.MinValue = MinSetValue;
        thisVarPtr.minValueDate = 0;
        thisVarPtr.Which = &ActualVariable;
        thisVarPtr.ReportID = state.dataOutputProcessor->CurrentReportNumber;
        thisVarPtr.ReportIDChr = IDOut.substr(0, 15);
        thisVarPtr.storeType = VariableType;
        thisVarPtr.Stored = false;
        thisVarPtr.Report = false;
        thisVarPtr.frequency = ReportingFrequency::Hourly;
        thisVarPtr.SchedPtr = 0;
        thisVarPtr.MeterArrayPtr = 0;
        thisVarPtr.ZoneMult = 1;
        thisVarPtr.ZoneListMult = 1;
        if (present(ZoneMult) && present(ZoneListMult)) {
            thisVarPtr.ZoneMult = ZoneMult;
            thisVarPtr.ZoneListMult = ZoneListMult;
        }

        if (Loop == 1) {
            if (OnMeter) {
                if (VariableType == StoreType::Averaged) {
                    ShowSevereError(state, "Meters can only be \"Summed\" variables");
                    ShowContinueError(state, "..reference variable=" + KeyedValue + ':' + VariableName);
                    ErrorsFound = true;
                } else {
                    Unit mtrUnits = state.dataOutputProcessor->RVariableTypes(CV).units;
                    ErrorsFound = false;
                    AttachMeters(state, mtrUnits, ResourceType, EndUse, EndUseSub, Group, ZoneName, CV, thisVarPtr.MeterArrayPtr, ErrorsFound);
                    if (ErrorsFound) {
                        ShowContinueError(state, "Invalid Meter spec for variable=" + KeyedValue + ':' + VariableName);
                        state.dataOutputProcessor->ErrorsLogged = true;
                    }
                }
            }
        }

        if (state.dataOutputProcessor->ReportList(Loop) == -1) continue;

        thisVarPtr.Report = true;

        if (state.dataOutputProcessor->ReportList(Loop) == 0) {
            thisVarPtr.frequency = RepFreq;
            thisVarPtr.SchedPtr = 0;
        } else {
            thisVarPtr.frequency = state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop)).frequency;
            thisVarPtr.SchedPtr = state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop)).SchedPtr;
        }

        if (thisVarPtr.Report) {
            if (present(indexGroupKey)) {
                localIndexGroupKey = indexGroupKey;
            } else {
                localIndexGroupKey = -999; // Unknown Group
            }

            if (thisVarPtr.SchedPtr != 0) {
                WriteReportVariableDictionaryItem(state,
                                                  thisVarPtr.frequency,
                                                  thisVarPtr.storeType,
                                                  thisVarPtr.ReportID,
                                                  localIndexGroupKey,
                                                  TimeStepTypeKey,
                                                  thisVarPtr.ReportIDChr,
                                                  KeyedValue,
                                                  VarName,
                                                  thisRvar.timeStepType,
                                                  thisRvar.units,
                                                  thisRvar.unitNameCustomEMS,
                                                  state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop)).SchedName);
            } else {
                WriteReportVariableDictionaryItem(state,
                                                  thisVarPtr.frequency,
                                                  thisVarPtr.storeType,
                                                  thisVarPtr.ReportID,
                                                  localIndexGroupKey,
                                                  TimeStepTypeKey,
                                                  thisVarPtr.ReportIDChr,
                                                  KeyedValue,
                                                  VarName,
                                                  thisRvar.timeStepType,
                                                  thisRvar.units,
                                                  thisRvar.unitNameCustomEMS);
            }
        }
    }
}

void SetupOutputVariable(EnergyPlusData &state,
                         std::string const &VariableName,           // String Name of variable
                         OutputProcessor::Unit const &VariableUnit, // Actual units corresponding to the actual variable
                         int &ActualVariable,                       // Actual Variable, used to set up pointer
                         std::string const &TimeStepTypeKey,        // Zone, HeatBalance=1, HVAC, System, Plant=2
                         std::string const &VariableTypeKey,        // State, Average=1, NonState, Sum=2
                         std::string const &KeyedValue,             // Associated Key for this variable
                         Optional_string_const ReportFreq,          // Internal use -- causes reporting at this freqency
                         Optional_int_const indexGroupKey           // Group identifier for SQL output
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
    int CV;
    TimeStepType TimeStepType; // 1=TimeStepZone, 2=TimeStepSys
    StoreType VariableType;    // 1=Average, 2=Sum, 3=Min/Max
    int localIndexGroupKey;
    int Loop;
    ReportingFrequency RepFreq(ReportingFrequency::Hourly);

    if (!state.dataOutputProcessor->OutputInitialized) InitializeOutput(state);

    // Variable name without units
    const std::string& VarName = VariableName;

    // Determine whether to Report or not
    CheckReportVariable(state, KeyedValue, VarName);

    if (state.dataOutputProcessor->NumExtraVars == 0) {
        state.dataOutputProcessor->NumExtraVars = 1;
        state.dataOutputProcessor->ReportList = -1;
    }

    // If ReportFreq present, overrides input
    if (present(ReportFreq)) {
        RepFreq = determineFrequency(state, ReportFreq);
        state.dataOutputProcessor->NumExtraVars = 1;
        state.dataOutputProcessor->ReportList = 0;
    }

    // DataOutputs::OutputVariablesForSimulation is case-insentitive
    bool const ThisOneOnTheList = DataOutputs::FindItemInVariableList(KeyedValue, VarName);

    for (Loop = 1; Loop <= state.dataOutputProcessor->NumExtraVars; ++Loop) {

        if (Loop == 1) ++state.dataOutputProcessor->NumOfIVariable_Setup;

        TimeStepType = ValidateTimeStepType(state, TimeStepTypeKey, "SetupOutputVariable");
        VariableType = validateVariableType(state, VariableTypeKey);

        AddToOutputVariableList(state, VarName, TimeStepType, VariableType, VarType_Integer, VariableUnit);
        ++state.dataOutputProcessor->NumTotalIVariable;

        if (!ThisOneOnTheList) continue;

        ++state.dataOutputProcessor->NumOfIVariable;
        if (Loop == 1 && VariableType == StoreType::Summed) {
            ++state.dataOutputProcessor->NumOfIVariable_Sum;
        }
        if (state.dataOutputProcessor->NumOfIVariable > state.dataOutputProcessor->MaxIVariable) {
            ReallocateIVar(state);
        }

        CV = state.dataOutputProcessor->NumOfIVariable;
        auto &thisIVar = state.dataOutputProcessor->IVariableTypes(CV);
        thisIVar.timeStepType = TimeStepType;
        thisIVar.storeType = VariableType;
        thisIVar.VarName = KeyedValue + ':' + VarName;
        thisIVar.VarNameOnly = VarName;
        thisIVar.VarNameOnlyUC = UtilityRoutines::MakeUPPERCase(VarName);
        thisIVar.VarNameUC = UtilityRoutines::MakeUPPERCase(thisIVar.VarName);
        thisIVar.KeyNameOnlyUC = UtilityRoutines::MakeUPPERCase(KeyedValue);
        thisIVar.units = VariableUnit;
        AssignReportNumber(state, state.dataOutputProcessor->CurrentReportNumber);
        const auto IDOut = fmt::to_string(state.dataOutputProcessor->CurrentReportNumber);
        thisIVar.ReportID = state.dataOutputProcessor->CurrentReportNumber;
        auto &thisVarPtr = thisIVar.VarPtr;
        thisVarPtr.Value = 0.0;
        thisVarPtr.StoreValue = 0.0;
        thisVarPtr.TSValue = 0.0;
        thisVarPtr.NumStored = 0.0;
        //    IVariable%LastTSValue=0
        thisVarPtr.MaxValue = IMaxSetValue;
        thisVarPtr.maxValueDate = 0;
        thisVarPtr.MinValue = IMinSetValue;
        thisVarPtr.minValueDate = 0;
        thisVarPtr.Which = &ActualVariable;
        thisVarPtr.ReportID = state.dataOutputProcessor->CurrentReportNumber;
        thisVarPtr.ReportIDChr = IDOut.substr(0, 15);
        thisVarPtr.storeType = VariableType;
        thisVarPtr.Stored = false;
        thisVarPtr.Report = false;
        thisVarPtr.frequency = ReportingFrequency::Hourly;
        thisVarPtr.SchedPtr = 0;

        if (state.dataOutputProcessor->ReportList(Loop) == -1) continue;

        thisVarPtr.Report = true;

        if (state.dataOutputProcessor->ReportList(Loop) == 0) {
            thisVarPtr.frequency = RepFreq;
            thisVarPtr.SchedPtr = 0;
        } else {
            thisVarPtr.frequency = state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop)).frequency;
            thisVarPtr.SchedPtr = state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop)).SchedPtr;
        }

        if (thisVarPtr.Report) {
            if (present(indexGroupKey)) {
                localIndexGroupKey = indexGroupKey;
            } else {
                localIndexGroupKey = -999; // Unknown Group
            }

            if (thisVarPtr.SchedPtr != 0) {
                WriteReportVariableDictionaryItem(state,
                                                  thisVarPtr.frequency,
                                                  thisVarPtr.storeType,
                                                  thisVarPtr.ReportID,
                                                  localIndexGroupKey,
                                                  TimeStepTypeKey,
                                                  thisVarPtr.ReportIDChr,
                                                  KeyedValue,
                                                  VarName,
                                                  thisIVar.timeStepType,
                                                  thisIVar.units,
                                                  state.dataOutputProcessor->ReqRepVars(state.dataOutputProcessor->ReportList(Loop)).SchedName);
            } else {
                WriteReportVariableDictionaryItem(state,
                                                  thisVarPtr.frequency,
                                                  thisVarPtr.storeType,
                                                  thisVarPtr.ReportID,
                                                  localIndexGroupKey,
                                                  TimeStepTypeKey,
                                                  thisVarPtr.ReportIDChr,
                                                  KeyedValue,
                                                  VarName,
                                                  thisIVar.timeStepType,
                                                  thisIVar.units);
            }
        }
    }
}

void SetupOutputVariable(EnergyPlusData &state,
                         std::string const &VariableName,           // String Name of variable
                         OutputProcessor::Unit const &VariableUnit, // Actual units corresponding to the actual variable
                         Real64 &ActualVariable,                    // Actual Variable, used to set up pointer
                         std::string const &TimeStepTypeKey,        // Zone, HeatBalance=1, HVAC, System, Plant=2
                         std::string const &VariableTypeKey,        // State, Average=1, NonState, Sum=2
                         int const KeyedValue,                      // Associated Key for this variable
                         Optional_string_const ReportFreq,          // Internal use -- causes reporting at this freqency
                         Optional_string_const ResourceTypeKey,     // Meter Resource Type (Electricity, Gas, etc)
                         Optional_string_const EndUseKey,           // Meter End Use Key (Lights, Heating, Cooling, etc)
                         Optional_string_const EndUseSubKey,        // Meter End Use Sub Key (General Lights, Task Lights, etc)
                         Optional_string_const GroupKey,            // Meter Super Group Key (Building, System, Plant)
                         Optional_string_const ZoneKey,             // Meter Zone Key (zone name)
                         Optional_int_const ZoneMult,               // Zone Multiplier, defaults to 1
                         Optional_int_const ZoneListMult,           // Zone List Multiplier, defaults to 1
                         Optional_int_const indexGroupKey           // Group identifier for SQL output
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   February 1999
    //       MODIFIED       January 2001; Implement Meters
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine allows an integer key for a variable.  Changes this to a
    // standard character variable and passes everything to SetupOutputVariable.

    // METHODOLOGY EMPLOYED:
    // Pointers (as pointers), pointers (as indices), and lots of other KEWL data stuff.

    // REFERENCES:
    // na

    // Using/Aliasing
    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    // Not checking for valid number

    SetupOutputVariable(state,
                        VariableName,
                        VariableUnit,
                        ActualVariable,
                        TimeStepTypeKey,
                        VariableTypeKey,
                        fmt::to_string(KeyedValue),
                        ReportFreq,
                        ResourceTypeKey,
                        EndUseKey,
                        EndUseSubKey,
                        GroupKey,
                        ZoneKey,
                        ZoneMult,
                        ZoneListMult,
                        indexGroupKey);
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

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;
    using General::EncodeMonDayHrMin;
    using ScheduleManager::GetCurrentScheduleValue;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;             // Loop Variable
    Real64 CurVal;        // Current value for real variables
    Real64 ICurVal;       // Current value for integer variables
    int MDHM;             // Month,Day,Hour,Minute
    bool TimePrint(true); // True if the time needs to be printed
    Real64 StartMinute;   // StartMinute for UpdateData call
    Real64 MinuteNow;     // What minute it is now
    bool ReportNow;       // True if this variable should be reported now
    int CurDayType;       // What kind of day it is (weekday (sunday, etc) or holiday)
    //////////// hoisted into namespace ////////////////////////////////////////////////
    // static int LHourP( -1 ); // Helps set hours for timestamp output
    // static Real64 LStartMin( -1.0 ); // Helps set minutes for timestamp output
    // static Real64 LEndMin( -1.0 ); // Helps set minutes for timestamp output
    ////////////////////////////////////////////////////////////////////////////////////
    static bool EndTimeStepFlag(false); // True when it's the end of the Zone Time Step
    Real64 rxTime;                      // (MinuteNow-StartMinute)/REAL(MinutesPerTimeStep,r64) - for execution time

    if (t_TimeStepTypeKey != TimeStepType::TimeStepZone && t_TimeStepTypeKey != TimeStepType::TimeStepSystem) {
        ShowFatalError(state, "Invalid reporting requested -- UpdateDataAndReport");
    }

    // Basic record keeping and report out if "detailed"
    StartMinute = state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).CurMinute;
    state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).CurMinute += (*state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).TimeStep) * 60.0;
    if (t_TimeStepTypeKey == TimeStepType::TimeStepSystem &&
        (state.dataOutputProcessor->TimeValue.at(TimeStepType::TimeStepSystem).CurMinute == state.dataOutputProcessor->TimeValue.at(TimeStepType::TimeStepZone).CurMinute)) {
        EndTimeStepFlag = true;
    } else if (t_TimeStepTypeKey == TimeStepType::TimeStepZone) {
        EndTimeStepFlag = true;
    } else {
        EndTimeStepFlag = false;
    }
    MinuteNow = state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).CurMinute;

    EncodeMonDayHrMin(MDHM, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, int(MinuteNow));
    TimePrint = true;

    rxTime = (MinuteNow - StartMinute) / double(state.dataGlobal->MinutesPerTimeStep);

    if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
        // R and I data frames for TimeStepType::TimeStepZone
        if (t_TimeStepTypeKey == TimeStepType::TimeStepZone && !ResultsFramework::resultsFramework->RIDetailedZoneTSData.rVariablesScanned()) {
            ResultsFramework::resultsFramework->initializeRTSDataFrame(
                ReportingFrequency::EachCall, state.dataOutputProcessor->RVariableTypes, state.dataOutputProcessor->NumOfRVariable, TimeStepType::TimeStepZone);
        }
        if (t_TimeStepTypeKey == TimeStepType::TimeStepZone && !ResultsFramework::resultsFramework->RIDetailedZoneTSData.iVariablesScanned()) {
            ResultsFramework::resultsFramework->initializeITSDataFrame(
                ReportingFrequency::EachCall, state.dataOutputProcessor->IVariableTypes, state.dataOutputProcessor->NumOfIVariable, TimeStepType::TimeStepZone);
        }

        // R and I data frames for TimeStepType::TimeStepSystem
        if (t_TimeStepTypeKey == TimeStepType::TimeStepSystem && !ResultsFramework::resultsFramework->RIDetailedHVACTSData.rVariablesScanned()) {
            ResultsFramework::resultsFramework->initializeRTSDataFrame(
                ReportingFrequency::EachCall, state.dataOutputProcessor->RVariableTypes, state.dataOutputProcessor->NumOfRVariable, TimeStepType::TimeStepSystem);
        }
        if (t_TimeStepTypeKey == TimeStepType::TimeStepSystem && !ResultsFramework::resultsFramework->RIDetailedHVACTSData.iVariablesScanned()) {
            ResultsFramework::resultsFramework->initializeITSDataFrame(
                ReportingFrequency::EachCall, state.dataOutputProcessor->IVariableTypes, state.dataOutputProcessor->NumOfIVariable, TimeStepType::TimeStepSystem);
        }
    }

    if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
        if (t_TimeStepTypeKey == TimeStepType::TimeStepZone) {
            ResultsFramework::resultsFramework->RIDetailedZoneTSData.newRow(state,
                                                                            state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, state.dataOutputProcessor->TimeValue.at(TimeStepType::TimeStepZone).CurMinute);
        }
        if (t_TimeStepTypeKey == TimeStepType::TimeStepSystem) {
            // TODO this was an error probably, was using TimeValue(1)
            ResultsFramework::resultsFramework->RIDetailedHVACTSData.newRow(state,
                                                                            state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, state.dataOutputProcessor->TimeValue.at(TimeStepType::TimeStepSystem).CurMinute);
        }
    }

    // Main "Record Keeping" Loops for R and I variables
    for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
        if (state.dataOutputProcessor->RVariableTypes(Loop).timeStepType != t_TimeStepTypeKey) continue;

        // Act on the RVariables variable
        auto &rVar(state.dataOutputProcessor->RVariableTypes(Loop).VarPtr);
        rVar.Stored = true;
        if (rVar.storeType == StoreType::Averaged) {
            CurVal = (*rVar.Which) * rxTime;
            //        CALL SetMinMax(RVar%Which,MDHM,RVar%MaxValue,RVar%maxValueDate,RVar%MinValue,RVar%minValueDate)
            if ((*rVar.Which) > rVar.MaxValue) {
                rVar.MaxValue = (*rVar.Which);
                rVar.maxValueDate = MDHM;
            }
            if ((*rVar.Which) < rVar.MinValue) {
                rVar.MinValue = (*rVar.Which);
                rVar.minValueDate = MDHM;
            }
            rVar.TSValue += CurVal;
            rVar.EITSValue = rVar.TSValue; // CR - 8481 fix - 09/06/2011
        } else {
            //        CurVal=RVar%Which
            if ((*rVar.Which) > rVar.MaxValue) {
                rVar.MaxValue = (*rVar.Which);
                rVar.maxValueDate = MDHM;
            }
            if ((*rVar.Which) < rVar.MinValue) {
                rVar.MinValue = (*rVar.Which);
                rVar.minValueDate = MDHM;
            }
            rVar.TSValue += (*rVar.Which);
            rVar.EITSValue = rVar.TSValue; // CR - 8481 fix - 09/06/2011
        }

        // End of "record keeping"  Report if applicable
        if (!rVar.Report) continue;
        ReportNow = true;
        if (rVar.SchedPtr > 0) ReportNow = (GetCurrentScheduleValue(state, rVar.SchedPtr) != 0.0); // SetReportNow(RVar%SchedPtr)
        if (!ReportNow) continue;
        rVar.tsStored = true;
        if (!rVar.thisTSStored) {
            ++rVar.thisTSCount;
            rVar.thisTSStored = true;
        }

        if (rVar.frequency == ReportingFrequency::EachCall) {
            if (TimePrint) {
                if (state.dataOutputProcessor->LHourP != state.dataGlobal->HourOfDay || std::abs(state.dataOutputProcessor->LStartMin - StartMinute) > 0.001 ||
                    std::abs(state.dataOutputProcessor->LEndMin - state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).CurMinute) > 0.001) {
                    CurDayType = state.dataEnvrn->DayOfWeek;
                    if (state.dataEnvrn->HolidayIndex > 0) {
                        CurDayType = 7 + state.dataEnvrn->HolidayIndex;
                    }
                    WriteTimeStampFormatData(state,
                                             state.files.eso,
                                             ReportingFrequency::EachCall,
                                             state.dataOutputProcessor->TimeStepStampReportNbr,
                                             state.dataOutputProcessor->TimeStepStampReportChr,
                                             state.dataGlobal->DayOfSimChr,
                                             true,
                                             state.dataEnvrn->Month,
                                             state.dataEnvrn->DayOfMonth,
                                             state.dataGlobal->HourOfDay,
                                             state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).CurMinute,
                                             StartMinute,
                                             state.dataEnvrn->DSTIndicator,
                                             DayTypes(CurDayType));
                    state.dataOutputProcessor->LHourP = state.dataGlobal->HourOfDay;
                    state.dataOutputProcessor->LStartMin = StartMinute;
                    state.dataOutputProcessor->LEndMin = state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).CurMinute;
                }
                TimePrint = false;
            }
            WriteNumericData(state, rVar.ReportID, rVar.ReportIDChr, *rVar.Which);
            ++state.dataGlobal->StdOutputRecordCount;

            if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
                if (t_TimeStepTypeKey == TimeStepType::TimeStepZone) {
                    ResultsFramework::resultsFramework->RIDetailedZoneTSData.pushVariableValue(rVar.ReportID, *rVar.Which);
                }
                if (t_TimeStepTypeKey == TimeStepType::TimeStepSystem) {
                    ResultsFramework::resultsFramework->RIDetailedHVACTSData.pushVariableValue(rVar.ReportID, *rVar.Which);
                }
            }
        }
    }

    for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
        if (state.dataOutputProcessor->IVariableTypes(Loop).timeStepType != t_TimeStepTypeKey) continue;

        // Act on the IVariables variable
        auto &iVar(state.dataOutputProcessor->IVariableTypes(Loop).VarPtr);
        iVar.Stored = true;
        //      ICurVal=IVar%Which
        if (iVar.storeType == StoreType::Averaged) {
            ICurVal = (*iVar.Which) * rxTime;
            iVar.TSValue += ICurVal;
            iVar.EITSValue = iVar.TSValue; // CR - 8481 fix - 09/06/2011
            if (nint(ICurVal) > iVar.MaxValue) {
                iVar.MaxValue = nint(ICurVal); // Record keeping for date and time go here too
                iVar.maxValueDate = MDHM;      //+ TimeValue.at(t_TimeStepTypeKey)%TimeStep
            }
            if (nint(ICurVal) < iVar.MinValue) {
                iVar.MinValue = nint(ICurVal);
                iVar.minValueDate = MDHM; //+ TimeValue.at(t_TimeStepTypeKey)%TimeStep
            }
        } else {
            if ((*iVar.Which) > iVar.MaxValue) {
                iVar.MaxValue = (*iVar.Which); // Record keeping for date and time go here too
                iVar.maxValueDate = MDHM;   //+ TimeValue(TimeStepType)%TimeStep
            }
            if ((*iVar.Which) < iVar.MinValue) {
                iVar.MinValue = (*iVar.Which);
                iVar.minValueDate = MDHM; //+ TimeValue(TimeStepType)%TimeStep
            }
            iVar.TSValue += (*iVar.Which);
            iVar.EITSValue = iVar.TSValue; // CR - 8481 fix - 09/06/2011
        }

        if (!iVar.Report) continue;
        ReportNow = true;
        if (iVar.SchedPtr > 0) ReportNow = (GetCurrentScheduleValue(state, iVar.SchedPtr) != 0.0); // SetReportNow(IVar%SchedPtr)
        if (!ReportNow) continue;
        iVar.tsStored = true;
        if (!iVar.thisTSStored) {
            ++iVar.thisTSCount;
            iVar.thisTSStored = true;
        }

        if (iVar.frequency == ReportingFrequency::EachCall) {
            if (TimePrint) {
                if (state.dataOutputProcessor->LHourP != state.dataGlobal->HourOfDay || std::abs(state.dataOutputProcessor->LStartMin - StartMinute) > 0.001 ||
                    std::abs(state.dataOutputProcessor->LEndMin - state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).CurMinute) > 0.001) {
                    CurDayType = state.dataEnvrn->DayOfWeek;
                    if (state.dataEnvrn->HolidayIndex > 0) {
                        CurDayType = 7 + state.dataEnvrn->HolidayIndex;
                    }
                    WriteTimeStampFormatData(state,
                                             state.files.eso,
                                             ReportingFrequency::EachCall,
                                             state.dataOutputProcessor->TimeStepStampReportNbr,
                                             state.dataOutputProcessor->TimeStepStampReportChr,
                                             state.dataGlobal->DayOfSimChr,
                                             true,
                                             state.dataEnvrn->Month,
                                             state.dataEnvrn->DayOfMonth,
                                             state.dataGlobal->HourOfDay,
                                             state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).CurMinute,
                                             StartMinute,
                                             state.dataEnvrn->DSTIndicator,
                                             DayTypes(CurDayType));
                    state.dataOutputProcessor->LHourP = state.dataGlobal->HourOfDay;
                    state.dataOutputProcessor->LStartMin = StartMinute;
                    state.dataOutputProcessor->LEndMin = state.dataOutputProcessor->TimeValue.at(t_TimeStepTypeKey).CurMinute;
                }
                TimePrint = false;
            }
            // only time integer vars actual report as integer only is "detailed"
            WriteNumericData(state, iVar.ReportID, iVar.ReportIDChr, *iVar.Which);
            ++state.dataGlobal->StdOutputRecordCount;

            if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
                if (t_TimeStepTypeKey == TimeStepType::TimeStepZone) {
                    ResultsFramework::resultsFramework->RIDetailedZoneTSData.pushVariableValue(iVar.ReportID, *iVar.Which);
                }
                if (t_TimeStepTypeKey == TimeStepType::TimeStepSystem) {
                    ResultsFramework::resultsFramework->RIDetailedHVACTSData.pushVariableValue(iVar.ReportID, *iVar.Which);
                }
            }
        }
    }

    if (t_TimeStepTypeKey == TimeStepType::TimeStepSystem) return; // All other stuff happens at the "zone" time step call to this routine.

    // TimeStep Block (Report on Zone TimeStep)

    if (EndTimeStepFlag) {
        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
            if (!ResultsFramework::resultsFramework->RITimestepTSData.rVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeRTSDataFrame(ReportingFrequency::TimeStep, state.dataOutputProcessor->RVariableTypes, state.dataOutputProcessor->NumOfRVariable);
            }
            if (!ResultsFramework::resultsFramework->RITimestepTSData.iVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeITSDataFrame(ReportingFrequency::TimeStep, state.dataOutputProcessor->IVariableTypes, state.dataOutputProcessor->NumOfIVariable);
            }
            ResultsFramework::resultsFramework->RITimestepTSData.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, state.dataOutputProcessor->TimeValue.at(TimeStepType::TimeStepZone).CurMinute);
        }

        for (auto &thisTimeStepType : {TimeStepType::TimeStepZone, TimeStepType::TimeStepSystem}) { // Zone, HVAC
            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
                if (state.dataOutputProcessor->RVariableTypes(Loop).timeStepType != thisTimeStepType) continue;
                auto &rVar(state.dataOutputProcessor->RVariableTypes(Loop).VarPtr);
                // Update meters on the TimeStep  (Zone)
                if (rVar.MeterArrayPtr != 0) {
                    if (state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).NumOnCustomMeters <= 0) {
                        UpdateMeterValues(state, rVar.TSValue * rVar.ZoneMult * rVar.ZoneListMult,
                                          state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).NumOnMeters,
                                          state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).OnMeters);
                    } else {
                        UpdateMeterValues(state, rVar.TSValue * rVar.ZoneMult * rVar.ZoneListMult,
                                          state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).NumOnMeters,
                                          state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).OnMeters,
                                          state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).NumOnCustomMeters,
                                          state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).OnCustomMeters);
                    }
                }
                ReportNow = true;
                if (rVar.SchedPtr > 0) ReportNow = (GetCurrentScheduleValue(state, rVar.SchedPtr) != 0.0); // SetReportNow(RVar%SchedPtr)
                if (!ReportNow || !rVar.Report) {
                    rVar.TSValue = 0.0;
                }
                //        IF (RVar%StoreType == AveragedVar) THEN
                //          RVar%Value=RVar%Value+RVar%TSValue/NumOfTimeStepInHour
                //        ELSE
                rVar.Value += rVar.TSValue;
                //        ENDIF

                if (!ReportNow || !rVar.Report) continue;

                if (rVar.frequency == ReportingFrequency::TimeStep) {
                    if (TimePrint) {
                        if (state.dataOutputProcessor->LHourP != state.dataGlobal->HourOfDay || std::abs(state.dataOutputProcessor->LStartMin - StartMinute) > 0.001 ||
                            std::abs(state.dataOutputProcessor->LEndMin - state.dataOutputProcessor->TimeValue.at(thisTimeStepType).CurMinute) > 0.001) {
                            CurDayType = state.dataEnvrn->DayOfWeek;
                            if (state.dataEnvrn->HolidayIndex > 0) {
                                CurDayType = 7 + state.dataEnvrn->HolidayIndex;
                            }
                            WriteTimeStampFormatData(state,
                                                     state.files.eso,
                                                     ReportingFrequency::EachCall,
                                                     state.dataOutputProcessor->TimeStepStampReportNbr,
                                                     state.dataOutputProcessor->TimeStepStampReportChr,
                                                     state.dataGlobal->DayOfSimChr,
                                                     true,
                                                     state.dataEnvrn->Month,
                                                     state.dataEnvrn->DayOfMonth,
                                                     state.dataGlobal->HourOfDay,
                                                     state.dataOutputProcessor->TimeValue.at(thisTimeStepType).CurMinute,
                                                     StartMinute,
                                                     state.dataEnvrn->DSTIndicator,
                                                     DayTypes(CurDayType));
                            state.dataOutputProcessor->LHourP = state.dataGlobal->HourOfDay;
                            state.dataOutputProcessor->LStartMin = StartMinute;
                            state.dataOutputProcessor->LEndMin = state.dataOutputProcessor->TimeValue.at(thisTimeStepType).CurMinute;
                        }
                        TimePrint = false;
                    }

                    WriteNumericData(state, rVar.ReportID, rVar.ReportIDChr, rVar.TSValue);
                    ++state.dataGlobal->StdOutputRecordCount;

                    if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
                        ResultsFramework::resultsFramework->RITimestepTSData.pushVariableValue(rVar.ReportID, rVar.TSValue);
                    }
                }
                rVar.TSValue = 0.0;
                rVar.thisTSStored = false;
            } // Number of R Variables

            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
                if (state.dataOutputProcessor->IVariableTypes(Loop).timeStepType != thisTimeStepType) continue;
                auto &iVar(state.dataOutputProcessor->IVariableTypes(Loop).VarPtr);
                ReportNow = true;
                if (iVar.SchedPtr > 0) ReportNow = (GetCurrentScheduleValue(state, iVar.SchedPtr) != 0.0); // SetReportNow(IVar%SchedPtr)
                if (!ReportNow) {
                    iVar.TSValue = 0.0;
                }
                //        IF (IVar%StoreType == AveragedVar) THEN
                //          IVar%Value=IVar%Value+REAL(IVar%TSValue,r64)/REAL(NumOfTimeStepInHour,r64)
                //        ELSE
                iVar.Value += iVar.TSValue;
                //        ENDIF

                if (!ReportNow || !iVar.Report) continue;

                if (iVar.frequency == ReportingFrequency::TimeStep) {
                    if (TimePrint) {
                        if (state.dataOutputProcessor->LHourP != state.dataGlobal->HourOfDay || std::abs(state.dataOutputProcessor->LStartMin - StartMinute) > 0.001 ||
                            std::abs(state.dataOutputProcessor->LEndMin - state.dataOutputProcessor->TimeValue.at(thisTimeStepType).CurMinute) > 0.001) {
                            CurDayType = state.dataEnvrn->DayOfWeek;
                            if (state.dataEnvrn->HolidayIndex > 0) {
                                CurDayType = 7 + state.dataEnvrn->HolidayIndex;
                            }
                            WriteTimeStampFormatData(state,
                                                     state.files.eso,
                                                     ReportingFrequency::EachCall,
                                                     state.dataOutputProcessor->TimeStepStampReportNbr,
                                                     state.dataOutputProcessor->TimeStepStampReportChr,
                                                     state.dataGlobal->DayOfSimChr,
                                                     true,
                                                     state.dataEnvrn->Month,
                                                     state.dataEnvrn->DayOfMonth,
                                                     state.dataGlobal->HourOfDay,
                                                     state.dataOutputProcessor->TimeValue.at(thisTimeStepType).CurMinute,
                                                     StartMinute,
                                                     state.dataEnvrn->DSTIndicator,
                                                     DayTypes(CurDayType));
                            state.dataOutputProcessor->LHourP = state.dataGlobal->HourOfDay;
                            state.dataOutputProcessor->LStartMin = StartMinute;
                            state.dataOutputProcessor->LEndMin = state.dataOutputProcessor->TimeValue.at(thisTimeStepType).CurMinute;
                        }
                        TimePrint = false;
                    }

                    WriteNumericData(state, iVar.ReportID, iVar.ReportIDChr, iVar.TSValue);
                    ++state.dataGlobal->StdOutputRecordCount;

                    if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
                        ResultsFramework::resultsFramework->RITimestepTSData.pushVariableValue(iVar.ReportID, iVar.TSValue);
                    }
                }
                iVar.TSValue = 0.0;
                iVar.thisTSStored = false;
            } // Number of I Variables
        }     // Index Type (Zone or HVAC)

        UpdateMeters(state, MDHM);

        ReportTSMeters(state, StartMinute, state.dataOutputProcessor->TimeValue.at(TimeStepType::TimeStepZone).CurMinute, TimePrint, TimePrint);

    } // TimeStep Block

    // Hour Block
    if (state.dataGlobal->EndHourFlag) {
        if (state.dataOutputProcessor->TrackingHourlyVariables) {
            CurDayType = state.dataEnvrn->DayOfWeek;
            if (state.dataEnvrn->HolidayIndex > 0) {
                CurDayType = 7 + state.dataEnvrn->HolidayIndex;
            }
            WriteTimeStampFormatData(state,
                                     state.files.eso,
                                     ReportingFrequency::Hourly,
                                     state.dataOutputProcessor->TimeStepStampReportNbr,
                                     state.dataOutputProcessor->TimeStepStampReportChr,
                                     state.dataGlobal->DayOfSimChr,
                                     true,
                                     state.dataEnvrn->Month,
                                     state.dataEnvrn->DayOfMonth,
                                     state.dataGlobal->HourOfDay,
                                     _,
                                     _,
                                     state.dataEnvrn->DSTIndicator,
                                     DayTypes(CurDayType));
            TimePrint = false;
        }

        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
            if (!ResultsFramework::resultsFramework->RIHourlyTSData.rVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeRTSDataFrame(ReportingFrequency::Hourly, state.dataOutputProcessor->RVariableTypes, state.dataOutputProcessor->NumOfRVariable);
            }
            if (!ResultsFramework::resultsFramework->RIHourlyTSData.iVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeITSDataFrame(ReportingFrequency::Hourly, state.dataOutputProcessor->IVariableTypes, state.dataOutputProcessor->NumOfIVariable);
            }
            ResultsFramework::resultsFramework->RIHourlyTSData.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
        }

        for (auto &thisTimeStepType : {TimeStepType::TimeStepZone, TimeStepType::TimeStepSystem}) { // Zone, HVAC
            state.dataOutputProcessor->TimeValue.at(thisTimeStepType).CurMinute = 0.0;
            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
                if (state.dataOutputProcessor->RVariableTypes(Loop).timeStepType != thisTimeStepType) continue;
                auto &rVar(state.dataOutputProcessor->RVariableTypes(Loop).VarPtr);
                //        ReportNow=.TRUE.
                //        IF (RVar%SchedPtr > 0) &
                //          ReportNow=(GetCurrentScheduleValue(state, RVar%SchedPtr) /= 0.0)  !SetReportNow(RVar%SchedPtr)

                //        IF (ReportNow) THEN
                if (rVar.tsStored) {
                    if (rVar.storeType == StoreType::Averaged) {
                        rVar.Value /= double(rVar.thisTSCount);
                    }
                    if (rVar.Report && rVar.frequency == ReportingFrequency::Hourly && rVar.Stored) {
                        WriteNumericData(state, rVar.ReportID, rVar.ReportIDChr, rVar.Value);
                        ++state.dataGlobal->StdOutputRecordCount;
                        rVar.Stored = false;
                        // add time series value for hourly to data store
                        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
                            ResultsFramework::resultsFramework->RIHourlyTSData.pushVariableValue(rVar.ReportID, rVar.Value);
                        }
                    }
                    rVar.StoreValue += rVar.Value;
                    ++rVar.NumStored;
                }
                rVar.tsStored = false;
                rVar.thisTSStored = false;
                rVar.thisTSCount = 0;
                rVar.Value = 0.0;
            } // Number of R Variables

            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
                if (state.dataOutputProcessor->IVariableTypes(Loop).timeStepType != thisTimeStepType) continue;
                auto &iVar(state.dataOutputProcessor->IVariableTypes(Loop).VarPtr);
                //        ReportNow=.TRUE.
                //        IF (IVar%SchedPtr > 0) &
                //          ReportNow=(GetCurrentScheduleValue(state, IVar%SchedPtr) /= 0.0)  !SetReportNow(IVar%SchedPtr)
                //        IF (ReportNow) THEN
                if (iVar.tsStored) {
                    if (iVar.storeType == StoreType::Averaged) {
                        iVar.Value /= double(iVar.thisTSCount);
                    }
                    if (iVar.Report && iVar.frequency == ReportingFrequency::Hourly && iVar.Stored) {
                        WriteNumericData(state, iVar.ReportID, iVar.ReportIDChr, iVar.Value);
                        ++state.dataGlobal->StdOutputRecordCount;
                        iVar.Stored = false;
                        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
                            ResultsFramework::resultsFramework->RIHourlyTSData.pushVariableValue(iVar.ReportID, iVar.Value);
                        }
                    }
                    iVar.StoreValue += iVar.Value;
                    ++iVar.NumStored;
                }
                iVar.tsStored = false;
                iVar.thisTSStored = false;
                iVar.thisTSCount = 0;
                iVar.Value = 0.0;
            } // Number of I Variables
        }     // thisTimeStepType (Zone or HVAC)

        ReportHRMeters(state, TimePrint);

    } // Hour Block

    if (!state.dataGlobal->EndHourFlag) return;

    // Day Block
    if (state.dataGlobal->EndDayFlag) {
        if (state.dataOutputProcessor->TrackingDailyVariables) {
            CurDayType = state.dataEnvrn->DayOfWeek;
            if (state.dataEnvrn->HolidayIndex > 0) {
                CurDayType = 7 + state.dataEnvrn->HolidayIndex;
            }
            WriteTimeStampFormatData(state,
                                     state.files.eso,
                                     ReportingFrequency::Daily,
                                     state.dataOutputProcessor->DailyStampReportNbr,
                                     state.dataOutputProcessor->DailyStampReportChr,
                                     state.dataGlobal->DayOfSimChr,
                                     true,
                                     state.dataEnvrn->Month,
                                     state.dataEnvrn->DayOfMonth,
                                     _,
                                     _,
                                     _,
                                     state.dataEnvrn->DSTIndicator,
                                     DayTypes(CurDayType));
            TimePrint = false;
        }
        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
            if (!ResultsFramework::resultsFramework->RIDailyTSData.rVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeRTSDataFrame(ReportingFrequency::Daily, state.dataOutputProcessor->RVariableTypes, state.dataOutputProcessor->NumOfRVariable);
            }
            if (!ResultsFramework::resultsFramework->RIDailyTSData.iVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeITSDataFrame(ReportingFrequency::Daily, state.dataOutputProcessor->IVariableTypes, state.dataOutputProcessor->NumOfIVariable);
            }
            ResultsFramework::resultsFramework->RIDailyTSData.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
        }

        state.dataOutputProcessor->NumHoursInMonth += 24;
        for (auto &thisTimeStepType : {TimeStepType::TimeStepZone, TimeStepType::TimeStepSystem}) { // Zone, HVAC
            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
                if (state.dataOutputProcessor->RVariableTypes(Loop).timeStepType == thisTimeStepType) {
                    WriteRealVariableOutput(state, state.dataOutputProcessor->RVariableTypes(Loop).VarPtr, ReportingFrequency::Daily);
                }
            } // Number of R Variables

            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
                if (state.dataOutputProcessor->IVariableTypes(Loop).timeStepType == thisTimeStepType) {
                    WriteIntegerVariableOutput(state, state.dataOutputProcessor->IVariableTypes(Loop).VarPtr, ReportingFrequency::Daily);
                }
            } // Number of I Variables
        }     // thisTimeStepType (Zone or HVAC)

        ReportDYMeters(state, TimePrint);

    } // Day Block

    // Only continue if EndDayFlag is set
    if (!state.dataGlobal->EndDayFlag) return;

    // Month Block
    if (state.dataEnvrn->EndMonthFlag || state.dataGlobal->EndEnvrnFlag) {
        if (state.dataOutputProcessor->TrackingMonthlyVariables) {
            WriteTimeStampFormatData(state,
                                     state.files.eso,
                                     ReportingFrequency::Monthly,
                                     state.dataOutputProcessor->MonthlyStampReportNbr,
                                     state.dataOutputProcessor->MonthlyStampReportChr,
                                     state.dataGlobal->DayOfSimChr,
                                     true,
                                     state.dataEnvrn->Month);
            TimePrint = false;
        }

        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
            if (!ResultsFramework::resultsFramework->RIMonthlyTSData.rVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeRTSDataFrame(ReportingFrequency::Monthly, state.dataOutputProcessor->RVariableTypes, state.dataOutputProcessor->NumOfRVariable);
            }
            if (!ResultsFramework::resultsFramework->RIMonthlyTSData.iVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeITSDataFrame(ReportingFrequency::Monthly, state.dataOutputProcessor->IVariableTypes, state.dataOutputProcessor->NumOfIVariable);
            }
            ResultsFramework::resultsFramework->RIMonthlyTSData.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
        }

        state.dataOutputProcessor->NumHoursInSim += state.dataOutputProcessor->NumHoursInMonth;
        state.dataEnvrn->EndMonthFlag = false;
        for (auto &thisTimeStepType : {TimeStepType::TimeStepZone, TimeStepType::TimeStepSystem}) { // Zone, HVAC
            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
                if (state.dataOutputProcessor->RVariableTypes(Loop).timeStepType == thisTimeStepType) {
                    WriteRealVariableOutput(state, state.dataOutputProcessor->RVariableTypes(Loop).VarPtr, ReportingFrequency::Monthly);
                }
            } // Number of R Variables

            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
                if (state.dataOutputProcessor->IVariableTypes(Loop).timeStepType == thisTimeStepType) {
                    WriteIntegerVariableOutput(state, state.dataOutputProcessor->IVariableTypes(Loop).VarPtr, ReportingFrequency::Monthly);
                }
            } // Number of I Variables
        }     // thisTimeStepType (Zone, HVAC)

        ReportMNMeters(state, TimePrint);

        state.dataOutputProcessor->NumHoursInMonth = 0;
    } // Month Block

    // Sim/Environment Block
    if (state.dataGlobal->EndEnvrnFlag) {
        if (state.dataOutputProcessor->TrackingRunPeriodVariables) {
            WriteTimeStampFormatData(state,
                                     state.files.eso,
                                     ReportingFrequency::Simulation,
                                     state.dataOutputProcessor->RunPeriodStampReportNbr,
                                     state.dataOutputProcessor->RunPeriodStampReportChr,
                                     state.dataGlobal->DayOfSimChr,
                                     true);
            TimePrint = false;
        }

        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
            if (!ResultsFramework::resultsFramework->RIRunPeriodTSData.rVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeRTSDataFrame(ReportingFrequency::Simulation, state.dataOutputProcessor->RVariableTypes, state.dataOutputProcessor->NumOfRVariable);
            }
            if (!ResultsFramework::resultsFramework->RIRunPeriodTSData.iVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeITSDataFrame(ReportingFrequency::Simulation, state.dataOutputProcessor->IVariableTypes, state.dataOutputProcessor->NumOfIVariable);
            }
            ResultsFramework::resultsFramework->RIRunPeriodTSData.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
        }
        for (auto &thisTimeStepType : {TimeStepType::TimeStepZone, TimeStepType::TimeStepSystem}) { // Zone, HVAC
            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
                if (state.dataOutputProcessor->RVariableTypes(Loop).timeStepType == thisTimeStepType) {
                    WriteRealVariableOutput(state, state.dataOutputProcessor->RVariableTypes(Loop).VarPtr, ReportingFrequency::Simulation);
                }
            } // Number of R Variables

            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
                if (state.dataOutputProcessor->IVariableTypes(Loop).timeStepType == thisTimeStepType) {
                    WriteIntegerVariableOutput(state, state.dataOutputProcessor->IVariableTypes(Loop).VarPtr, ReportingFrequency::Simulation);
                }
            } // Number of I Variables
        }     // thisTimeStepType (Zone, HVAC)

        ReportSMMeters(state, TimePrint);

        state.dataOutputProcessor->NumHoursInSim = 0;
    }

    // Yearly Block
    if (state.dataEnvrn->EndYearFlag) {
        if (state.dataOutputProcessor->TrackingYearlyVariables) {
            WriteYearlyTimeStamp(state, state.files.eso, state.dataOutputProcessor->YearlyStampReportChr, state.dataGlobal->CalendarYearChr, true);
            TimePrint = false;
        }
        if (ResultsFramework::resultsFramework->timeSeriesEnabled()) {
            if (!ResultsFramework::resultsFramework->RIYearlyTSData.rVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeRTSDataFrame(ReportingFrequency::Yearly, state.dataOutputProcessor->RVariableTypes, state.dataOutputProcessor->NumOfRVariable);
            }
            if (!ResultsFramework::resultsFramework->RIYearlyTSData.iVariablesScanned()) {
                ResultsFramework::resultsFramework->initializeITSDataFrame(ReportingFrequency::Yearly, state.dataOutputProcessor->IVariableTypes, state.dataOutputProcessor->NumOfIVariable);
            }
            ResultsFramework::resultsFramework->RIYearlyTSData.newRow(state, state.dataEnvrn->Month, state.dataEnvrn->DayOfMonth, state.dataGlobal->HourOfDay, 0);
        }
        for (auto &thisTimeStepType : {TimeStepType::TimeStepZone, TimeStepType::TimeStepSystem}) { // Zone, HVAC
            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
                if (state.dataOutputProcessor->RVariableTypes(Loop).timeStepType == thisTimeStepType) {
                    WriteRealVariableOutput(state, state.dataOutputProcessor->RVariableTypes(Loop).VarPtr, ReportingFrequency::Yearly);
                }
            } // Number of R Variables

            for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
                if (state.dataOutputProcessor->IVariableTypes(Loop).timeStepType == thisTimeStepType) {
                    WriteIntegerVariableOutput(state, state.dataOutputProcessor->IVariableTypes(Loop).VarPtr, ReportingFrequency::Yearly);
                }
            } // Number of I Variables
        }     // thisTimeStepType (Zone, HVAC)

        ReportYRMeters(state, TimePrint);

        state.dataGlobal->CalendarYear += 1;
        state.dataGlobal->CalendarYearChr = fmt::to_string(state.dataGlobal->CalendarYear);
    }
}

void AssignReportNumber(EnergyPlusData &state, int &ReportNumber)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine returns the next report number available.  The report number
    // is used in output reports as a key.

    // METHODOLOGY EMPLOYED:
    // Use internal ReportNumberCounter to maintain current report numbers.

    // REFERENCES:
    // na

    // USE STATEMENTS:
    using namespace OutputProcessor;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    //////////// hoisted into namespace ////////////
    // static int ReportNumberCounter( 0 );
    ////////////////////////////////////////////////

    ++state.dataOutputProcessor->ReportNumberCounter;
    ReportNumber = state.dataOutputProcessor->ReportNumberCounter;
}

void GenOutputVariablesAuditReport(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine reports (to the .err file) any report variables
    // which were requested but not "setup" during the run.  These will
    // either be items that were not used in the IDF file or misspellings
    // of report variable names.

    // METHODOLOGY EMPLOYED:
    // Use flagged data structure in OutputProcessor.

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::map<ReportingFrequency, std::string> reportFrequency({{ReportingFrequency::EachCall, "Detailed"},
                                                                      {ReportingFrequency::TimeStep, "TimeStep"},
                                                                      {ReportingFrequency::Hourly, "Hourly"},
                                                                      {ReportingFrequency::Daily, "Daily"},
                                                                      {ReportingFrequency::Monthly, "Monthly"},
                                                                      {ReportingFrequency::Yearly, "Annual"}});

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    static bool Rept(false);
    int Loop;
    static bool OpaqSurfWarned(false);

    for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfReqVariables; ++Loop) {
        if (state.dataOutputProcessor->ReqRepVars(Loop).Used) continue;
        if (state.dataOutputProcessor->ReqRepVars(Loop).Key.empty()) state.dataOutputProcessor->ReqRepVars(Loop).Key = "*";
        if (has(state.dataOutputProcessor->ReqRepVars(Loop).VarName, "OPAQUE SURFACE INSIDE FACE CONDUCTION") && !state.dataGlobal->DisplayAdvancedReportVariables && !OpaqSurfWarned) {
            ShowWarningError(state, "Variables containing \"Opaque Surface Inside Face Conduction\" are now \"advanced\" variables.");
            ShowContinueError(state, "You must enter the \"Output:Diagnostics,DisplayAdvancedReportVariables;\" statement to view.");
            ShowContinueError(state, "First, though, read cautionary statements in the \"InputOutputReference\" document.");
            OpaqSurfWarned = true;
        }
        if (!Rept) {
            ShowWarningError(state, "The following Report Variables were requested but not generated -- check.rdd file");
            ShowContinueError(state, "Either the IDF did not contain these elements, the variable name is misspelled,");
            ShowContinueError(state, "or the requested variable is an advanced output which requires Output : Diagnostics, DisplayAdvancedReportVariables;");
            Rept = true;
        }
        ShowMessage(state, "Key=" + state.dataOutputProcessor->ReqRepVars(Loop).Key + ", VarName=" + state.dataOutputProcessor->ReqRepVars(Loop).VarName +
                    ", Frequency=" + reportFrequency[state.dataOutputProcessor->ReqRepVars(Loop).frequency]);
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
    using namespace DataIPShortCuts;
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    Array1D_string Alphas(2);
    Array1D<Real64> Numbers(1);
    int NumAlpha;
    int NumNumbers;
    int IOStat;
    std::string::size_type varnameLen;
    int NumReqMeters;
    int NumReqMeterFOs;
    ReportingFrequency ReportFreq;

    static bool ErrorsFound(false); // If errors detected in input

    GetCustomMeterInput(state, ErrorsFound);
    if (ErrorsFound) {
        state.dataOutputProcessor->ErrorsLogged = true;
    }

    // Helper lambda to locate a meter index from its name. Returns a negative value if not found
    auto findMeterIndexFromMeterName = [&state](std::string const &name) -> int {
        // Return a value <= 0 if not found
        int meterIndex = -99;

        std::string::size_type wildCardPosition = index(name, '*');

        if (wildCardPosition == std::string::npos) {
            meterIndex = UtilityRoutines::FindItem(name, state.dataOutputProcessor->EnergyMeters);
        } else { // Wildcard input
            for (int Meter = 1; Meter <= state.dataOutputProcessor->NumEnergyMeters; ++Meter) {
                if (UtilityRoutines::SameString(state.dataOutputProcessor->EnergyMeters(Meter).Name.substr(0, wildCardPosition),
                                                name.substr(0, wildCardPosition)))
                {
                    meterIndex = Meter;
                    break;
                }
            }
        }

        return meterIndex;
    };

    cCurrentModuleObject = "Output:Meter";
    NumReqMeters = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    for (Loop = 1; Loop <= NumReqMeters; ++Loop) {

        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      Loop,
                                      Alphas,
                                      NumAlpha,
                                      Numbers,
                                      NumNumbers,
                                      IOStat,
                                      lNumericFieldBlanks,
                                      lAlphaFieldBlanks,
                                      cAlphaFieldNames,
                                      cNumericFieldNames);

        varnameLen = index(Alphas(1), '[');
        if (varnameLen != std::string::npos) Alphas(1).erase(varnameLen);

        ReportFreq = determineFrequency(state, Alphas(2));

        int meterIndex = findMeterIndexFromMeterName(Alphas(1));
        if (meterIndex > 0) {
            // MeterFileOnlyIndicator is false, CumulativeIndicator is false
            SetInitialMeterReportingAndOutputNames(state, meterIndex, false, ReportFreq, false);
        } else {
            ShowWarningError(state, cCurrentModuleObject + ": invalid " + cAlphaFieldNames(1) + "=\"" + Alphas(1) + "\" - not found.");
        }
    }

    cCurrentModuleObject = "Output:Meter:MeterFileOnly";
    NumReqMeterFOs = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    for (Loop = 1; Loop <= NumReqMeterFOs; ++Loop) {

        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      Loop,
                                      Alphas,
                                      NumAlpha,
                                      Numbers,
                                      NumNumbers,
                                      IOStat,
                                      lNumericFieldBlanks,
                                      lAlphaFieldBlanks,
                                      cAlphaFieldNames,
                                      cNumericFieldNames);

        varnameLen = index(Alphas(1), '[');
        if (varnameLen != std::string::npos) Alphas(1).erase(varnameLen);

        ReportFreq = determineFrequency(state, Alphas(2));

        int meterIndex = findMeterIndexFromMeterName(Alphas(1));
        if (meterIndex > 0) {
            // MeterFileOnlyIndicator is true, CumulativeIndicator is false
            SetInitialMeterReportingAndOutputNames(state, meterIndex, true, ReportFreq, false);
        } else {
            ShowWarningError(state, cCurrentModuleObject + ": invalid " + cAlphaFieldNames(1) + "=\"" + Alphas(1) + "\" - not found.");
        }
    }

    cCurrentModuleObject = "Output:Meter:Cumulative";
    NumReqMeters = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    for (Loop = 1; Loop <= NumReqMeters; ++Loop) {

        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      Loop,
                                      Alphas,
                                      NumAlpha,
                                      Numbers,
                                      NumNumbers,
                                      IOStat,
                                      lNumericFieldBlanks,
                                      lAlphaFieldBlanks,
                                      cAlphaFieldNames,
                                      cNumericFieldNames);

        varnameLen = index(Alphas(1), '[');
        if (varnameLen != std::string::npos) Alphas(1).erase(varnameLen);

        ReportFreq = determineFrequency(state, Alphas(2));

        int meterIndex = findMeterIndexFromMeterName(Alphas(1));
        if (meterIndex > 0) {
            // MeterFileOnlyIndicator is false, CumulativeIndicator is true
            SetInitialMeterReportingAndOutputNames(state, meterIndex, false, ReportFreq, true);
        } else {
            ShowWarningError(state, cCurrentModuleObject + ": invalid " + cAlphaFieldNames(1) + "=\"" + Alphas(1) + "\" - not found.");
        }
    }

    cCurrentModuleObject = "Output:Meter:Cumulative:MeterFileOnly";
    NumReqMeterFOs = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    for (Loop = 1; Loop <= NumReqMeterFOs; ++Loop) {

        inputProcessor->getObjectItem(state,
                                      cCurrentModuleObject,
                                      Loop,
                                      Alphas,
                                      NumAlpha,
                                      Numbers,
                                      NumNumbers,
                                      IOStat,
                                      lNumericFieldBlanks,
                                      lAlphaFieldBlanks,
                                      cAlphaFieldNames,
                                      cNumericFieldNames);

        varnameLen = index(Alphas(1), '[');
        if (varnameLen != std::string::npos) Alphas(1).erase(varnameLen);

        ReportFreq = determineFrequency(state, Alphas(2));

        int meterIndex = findMeterIndexFromMeterName(Alphas(1));
        if (meterIndex > 0) {
            // MeterFileOnlyIndicator is true, CumulativeIndicator is true
            SetInitialMeterReportingAndOutputNames(state, meterIndex, true, ReportFreq, true);
        } else {
            ShowWarningError(state, cCurrentModuleObject + ": invalid " + cAlphaFieldNames(1) + "=\"" + Alphas(1) + "\" - not found.");
        }
    }

    ReportMeterDetails(state);

    if (state.dataOutputProcessor->ErrorsLogged) {
        ShowFatalError(state, "UpdateMeterReporting: Previous Meter Specification errors cause program termination.");
    }

    state.dataOutputProcessor->MeterValue.dimension(state.dataOutputProcessor->NumEnergyMeters, 0.0);
}

void SetInitialMeterReportingAndOutputNames(EnergyPlusData &state,
                                            int const WhichMeter,              // Which meter number
                                            bool const MeterFileOnlyIndicator, // true if this is a meter file only reporting
                                            OutputProcessor::ReportingFrequency const FrequencyIndicator, // at what frequency is the meter reported
                                            bool const CumulativeIndicator // true if this is a Cumulative meter reporting
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Set values and output initial names to output files.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int indexGroupKey;
    std::string indexGroup;

    if ((FrequencyIndicator == ReportingFrequency::EachCall) ||
        (FrequencyIndicator == ReportingFrequency::TimeStep)) { // roll "detailed" into TimeStep
        if (!CumulativeIndicator) {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptTS) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"" + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (TimeStep), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptTS) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptTS = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptTSFO = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).TSRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).TSRptNumChr,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         false,
                                         MeterFileOnlyIndicator);
            }
        } else {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccTS) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"Cumulative " + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (TimeStep), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccTS) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccTS = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccTSFO = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).TSAccRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         fmt::to_string(state.dataOutputProcessor->EnergyMeters(WhichMeter).TSAccRptNum),
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         true,
                                         MeterFileOnlyIndicator);
            }
        }
    } else if (FrequencyIndicator == ReportingFrequency::Hourly) {
        if (!CumulativeIndicator) {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptHR) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"" + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (Hourly), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptHR) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptHR = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptHRFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingHourlyVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).HRRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).HRRptNumChr,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         false,
                                         MeterFileOnlyIndicator);
            }
        } else {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccHR) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"Cumulative " + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (Hourly), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccHR) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccHR = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccHRFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingHourlyVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).HRAccRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         fmt::to_string(state.dataOutputProcessor->EnergyMeters(WhichMeter).HRAccRptNum),
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         true,
                                         MeterFileOnlyIndicator);
            }
        }
    } else if (FrequencyIndicator == ReportingFrequency::Daily) {
        if (!CumulativeIndicator) {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptDY) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"" + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (Daily), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptDY) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptDY = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptDYFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingDailyVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).DYRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).DYRptNumChr,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         false,
                                         MeterFileOnlyIndicator);
            }
        } else {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccDY) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"Cumulative " + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (Hourly), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccDY) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccDY = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccDYFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingDailyVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).DYAccRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         fmt::to_string(state.dataOutputProcessor->EnergyMeters(WhichMeter).DYAccRptNum),
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         true,
                                         MeterFileOnlyIndicator);
            }
        }
    } else if (FrequencyIndicator == ReportingFrequency::Monthly) {
        if (!CumulativeIndicator) {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptMN) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"" + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (Monthly), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptMN) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptMN = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptMNFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingMonthlyVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).MNRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).MNRptNumChr,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         false,
                                         MeterFileOnlyIndicator);
            }
        } else {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccMN) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"Cumulative " + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (Monthly), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccMN) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccMN = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccMNFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingMonthlyVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).MNAccRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         fmt::to_string(state.dataOutputProcessor->EnergyMeters(WhichMeter).MNAccRptNum),
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         true,
                                         MeterFileOnlyIndicator);
            }
        }
    } else if (FrequencyIndicator == ReportingFrequency::Yearly) {
        if (!CumulativeIndicator) {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptYR) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"" + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (Annual), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptYR) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptYR = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptYRFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingYearlyVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).YRRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).YRRptNumChr,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         false,
                                         MeterFileOnlyIndicator);
            }
        } else {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccYR) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"Cumulative " + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (Annual), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccYR) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccYR = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccYRFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingYearlyVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).YRAccRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         fmt::to_string(state.dataOutputProcessor->EnergyMeters(WhichMeter).YRAccRptNum),
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         true,
                                         MeterFileOnlyIndicator);
            }
        }
    } else if (FrequencyIndicator == ReportingFrequency::Simulation) {
        if (!CumulativeIndicator) {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptSM) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"" + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (RunPeriod), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptSM) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptSM = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptSMFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingRunPeriodVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).SMRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).SMRptNumChr,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         false,
                                         MeterFileOnlyIndicator);
            }
        } else {
            if (MeterFileOnlyIndicator) {
                if (state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccSM) {
                    ShowWarningError(state, "Output:Meter:MeterFileOnly requested for \"Cumulative " + state.dataOutputProcessor->EnergyMeters(WhichMeter).Name +
                                     R"(" (RunPeriod), already on "Output:Meter". Will report to both )" + state.files.eso.fileName +
                                     " and " + state.files.mtr.fileName);
                }
            }
            if (!state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccSM) {
                state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccSM = true;
                if (MeterFileOnlyIndicator) state.dataOutputProcessor->EnergyMeters(WhichMeter).RptAccSMFO = true;
                if (!MeterFileOnlyIndicator) state.dataOutputProcessor->TrackingRunPeriodVariables = true;
                indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(WhichMeter).Name);
                indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(WhichMeter));
                WriteMeterDictionaryItem(state,
                                         FrequencyIndicator,
                                         StoreType::Summed,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).SMAccRptNum,
                                         indexGroupKey,
                                         indexGroup,
                                         fmt::to_string(state.dataOutputProcessor->EnergyMeters(WhichMeter).SMAccRptNum),
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Name,
                                         state.dataOutputProcessor->EnergyMeters(WhichMeter).Units,
                                         true,
                                         MeterFileOnlyIndicator);
            }
        }
    } else {
    }
}

int GetMeterIndex(EnergyPlusData &state, std::string const &MeterName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns a index to the meter "number" (aka assigned report number)
    // for the meter name.  If none active for this run, a zero is returned.  This is used later to
    // obtain a meter "value".

    // Using/Aliasing
    using namespace OutputProcessor;
    using SortAndStringUtilities::SetupAndSort;

    // Return value
    int MeterIndex;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    // Valid Meter names because matching case insensitive
    static Array1D_string ValidMeterNames;
    static Array1D_int iValidMeterNames;
    static int NumValidMeters(0);
    //////////// hoisted into namespace changed to GetMeterIndexFirstCall////////////
    // static bool FirstCall( true );
    ////////////////////////////////////////////////
    int Found;

    if (state.dataOutputProcessor->GetMeterIndexFirstCall || (NumValidMeters != state.dataOutputProcessor->NumEnergyMeters)) {
        NumValidMeters = state.dataOutputProcessor->NumEnergyMeters;
        ValidMeterNames.allocate(NumValidMeters);
        for (Found = 1; Found <= NumValidMeters; ++Found) {
            ValidMeterNames(Found) = UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->EnergyMeters(Found).Name);
        }
        iValidMeterNames.allocate(NumValidMeters);
        SetupAndSort(ValidMeterNames, iValidMeterNames);
        state.dataOutputProcessor->GetMeterIndexFirstCall = false;
    }

    MeterIndex = UtilityRoutines::FindItemInSortedList(MeterName, ValidMeterNames, NumValidMeters);
    if (MeterIndex != 0) MeterIndex = iValidMeterNames(MeterIndex);

    return MeterIndex;
}

std::string GetMeterResourceType(EnergyPlusData &state, int const MeterNumber) // Which Meter Number (from GetMeterIndex)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns the character string of Resource Type for the
    // given meter number/index. If MeterNumber is 0, ResourceType=Invalid/Unknown.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;

    // Return value
    std::string ResourceType;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    if (MeterNumber > 0) {
        ResourceType = state.dataOutputProcessor->EnergyMeters(MeterNumber).ResourceType;
    } else {
        ResourceType = "Invalid/Unknown";
    }

    return ResourceType;
}

Real64 GetCurrentMeterValue(EnergyPlusData &state, int const MeterNumber) // Which Meter Number (from GetMeterIndex)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns the current meter value (timestep) for the meter number indicated.

    // METHODOLOGY EMPLOYED:
    // Uses internal EnergyMeters structure to get value.

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;

    // Return value
    Real64 CurrentMeterValue;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    // na

    if (MeterNumber > 0) {
        CurrentMeterValue = state.dataOutputProcessor->EnergyMeters(MeterNumber).CurTSValue;
    } else {
        CurrentMeterValue = 0.0;
    }

    return CurrentMeterValue;
}

Real64 GetInstantMeterValue(EnergyPlusData &state,
                            int const MeterNumber,                             // Which Meter Number (from GetMeterIndex)
                            OutputProcessor::TimeStepType const t_timeStepType // Whether this is zone of HVAC
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   February 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns the Instantaneous meter value (timestep) for the meter number indicated
    //  using TimeStepType to differentiate between Zone and HVAC values.

    // METHODOLOGY EMPLOYED:
    // Uses internal EnergyMeters structure to get value.

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;

    // Return value
    Real64 InstantMeterValue(0.0);

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:

    //      EnergyMeters(Meter)%TSValue=EnergyMeters(EnergyMeters(Meter)%SourceMeter)%TSValue-MeterValue(Meter)

    if (MeterNumber == 0) return InstantMeterValue;

    auto &energy_meter(state.dataOutputProcessor->EnergyMeters(MeterNumber));
    auto &cache_beg(energy_meter.InstMeterCacheStart);
    auto &cache_end(energy_meter.InstMeterCacheEnd);
    if (energy_meter.TypeOfMeter != MeterType_CustomDec) {
        // section added to speed up the execution of this routine
        // instead of looping through all the VarMeterArrays to see if a RVariableType is used for a
        // specific meter, create a list of all the indexes for RVariableType that are used for that
        // meter.
        if (cache_beg == 0) { // not yet added to the cache
            for (int Loop = 1; Loop <= state.dataOutputProcessor->NumVarMeterArrays; ++Loop) {
                auto const &var_meter_on(state.dataOutputProcessor->VarMeterArrays(Loop).OnMeters);
                for (int Meter = 1, Meter_end = state.dataOutputProcessor->VarMeterArrays(Loop).NumOnMeters; Meter <= Meter_end; ++Meter) {
                    if (var_meter_on(Meter) == MeterNumber) {
                        IncrementInstMeterCache(state);
                        cache_end = state.dataOutputProcessor->InstMeterCacheLastUsed;
                        if (cache_beg == 0) cache_beg = state.dataOutputProcessor->InstMeterCacheLastUsed;
                        state.dataOutputProcessor->InstMeterCache(state.dataOutputProcessor->InstMeterCacheLastUsed) = state.dataOutputProcessor->VarMeterArrays(Loop).RepVariable;
                        break;
                    }
                }
                auto const &var_meter_on_custom(state.dataOutputProcessor->VarMeterArrays(Loop).OnCustomMeters);
                for (int Meter = 1, Meter_end = state.dataOutputProcessor->VarMeterArrays(Loop).NumOnCustomMeters; Meter <= Meter_end; ++Meter) {
                    if (var_meter_on_custom(Meter) == MeterNumber) {
                        IncrementInstMeterCache(state);
                        cache_end = state.dataOutputProcessor->InstMeterCacheLastUsed;
                        if (cache_beg == 0) cache_beg = state.dataOutputProcessor->InstMeterCacheLastUsed;
                        state.dataOutputProcessor->InstMeterCache(state.dataOutputProcessor->InstMeterCacheLastUsed) = state.dataOutputProcessor->VarMeterArrays(Loop).RepVariable;
                        break;
                    }
                } // End Number of Meters Loop
            }
        }
        for (int Loop = cache_beg; Loop <= cache_end; ++Loop) {
            auto &r_var_loop(state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->InstMeterCache(Loop)));
            // Separate the Zone variables from the HVAC variables using TimeStepType
            if (r_var_loop.timeStepType == t_timeStepType) {
                auto &rVar(r_var_loop.VarPtr);
                // Add to the total all of the appropriate variables
                InstantMeterValue += (*rVar.Which) * rVar.ZoneMult * rVar.ZoneListMult;
            }
        }
    } else { // MeterType_CustomDec
        // Get Source Meter value
        // Loop through all report meters to find correct report variables to add to instant meter total
        for (int Loop = 1; Loop <= state.dataOutputProcessor->NumVarMeterArrays; ++Loop) {
            auto &r_var_loop(state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(Loop).RepVariable));

            auto const &var_meter_on(state.dataOutputProcessor->VarMeterArrays(Loop).OnMeters);
            for (int Meter = 1, Meter_end = state.dataOutputProcessor->VarMeterArrays(Loop).NumOnMeters; Meter <= Meter_end; ++Meter) {
                if (var_meter_on(Meter) == energy_meter.SourceMeter) {
                    // Separate the Zone variables from the HVAC variables using TimeStepType
                    if (r_var_loop.timeStepType == t_timeStepType) {
                        auto &rVar(r_var_loop.VarPtr);
                        // Add to the total all of the appropriate variables
                        InstantMeterValue += (*rVar.Which) * rVar.ZoneMult * rVar.ZoneListMult;
                        break;
                    }
                }
            }

            auto const &var_meter_on_custom(state.dataOutputProcessor->VarMeterArrays(Loop).OnCustomMeters);
            for (int Meter = 1, Meter_end = state.dataOutputProcessor->VarMeterArrays(Loop).NumOnCustomMeters; Meter <= Meter_end; ++Meter) {
                if (var_meter_on_custom(Meter) == energy_meter.SourceMeter) {
                    // Separate the Zone variables from the HVAC variables using TimeStepType
                    if (r_var_loop.timeStepType == t_timeStepType) {
                        auto &rVar(r_var_loop.VarPtr);
                        // Add to the total all of the appropriate variables
                        InstantMeterValue += (*rVar.Which) * rVar.ZoneMult * rVar.ZoneListMult;
                        break;
                    }
                }
            }

        } // End Number of Meters Loop
        for (int Loop = 1; Loop <= state.dataOutputProcessor->NumVarMeterArrays; ++Loop) {
            auto &r_var_loop(state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->VarMeterArrays(Loop).RepVariable));

            auto const &var_meter_on(state.dataOutputProcessor->VarMeterArrays(Loop).OnMeters);
            for (int Meter = 1, Meter_end = state.dataOutputProcessor->VarMeterArrays(Loop).NumOnMeters; Meter <= Meter_end; ++Meter) {
                if (var_meter_on(Meter) == MeterNumber) {
                    // Separate the Zone variables from the HVAC variables using TimeStepType
                    if (r_var_loop.timeStepType == t_timeStepType) {
                        auto &rVar(r_var_loop.VarPtr);
                        // Add to the total all of the appropriate variables
                        InstantMeterValue -= (*rVar.Which) * rVar.ZoneMult * rVar.ZoneListMult;
                        break;
                    }
                }
            }

            auto const &var_meter_on_custom(state.dataOutputProcessor->VarMeterArrays(Loop).OnCustomMeters);
            for (int Meter = 1, Meter_end = state.dataOutputProcessor->VarMeterArrays(Loop).NumOnCustomMeters; Meter <= Meter_end; ++Meter) {
                if (var_meter_on_custom(Meter) == MeterNumber) {
                    // Separate the Zone variables from the HVAC variables using TimeStepType
                    if (r_var_loop.timeStepType == t_timeStepType) {
                        auto &rVar(r_var_loop.VarPtr);
                        // Add to the total all of the appropriate variables
                        InstantMeterValue -= (*rVar.Which) * rVar.ZoneMult * rVar.ZoneListMult;
                        break;
                    }
                }
            }

        } // End Number of Meters Loop
    }

    return InstantMeterValue;
}

void IncrementInstMeterCache(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   January 2013
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Manage the InstMeterCache array

    // METHODOLOGY EMPLOYED:
    // When the array grows to large, double it.

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;

    if (!allocated(state.dataOutputProcessor->InstMeterCache)) {
        state.dataOutputProcessor->InstMeterCache.dimension(state.dataOutputProcessor->InstMeterCacheSizeInc, 0); // zero the entire array
        state.dataOutputProcessor->InstMeterCacheLastUsed = 1;
    } else {
        ++state.dataOutputProcessor->InstMeterCacheLastUsed;
        // if larger than current size grow the array
        if (state.dataOutputProcessor->InstMeterCacheLastUsed > state.dataOutputProcessor->InstMeterCacheSize) {
            state.dataOutputProcessor->InstMeterCache.redimension(state.dataOutputProcessor->InstMeterCacheSize += state.dataOutputProcessor->InstMeterCacheSizeInc, 0);
        }
    }
}

Real64 GetInternalVariableValue(EnergyPlusData &state,
                                int const varType,    // 1=integer, 2=real, 3=meter
                                int const keyVarIndex // Array index
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   December 2000
    //       MODIFIED       August 2003, M. J. Witte
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns the current value of the Internal Variable assigned to
    // the varType and keyVarIndex.  Values may be accessed for REAL(r64) and integer
    // report variables and meter variables.  The variable type (varType) may be
    // determined by calling subroutine and GetVariableKeyCountandType.  The
    // index (keyVarIndex) may be determined by calling subroutine GetVariableKeys.

    // METHODOLOGY EMPLOYED:
    // Uses Internal OutputProcessor data structure to return value.

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;
    using ScheduleManager::GetCurrentScheduleValue;

    // Return value
    Real64 resultVal; // value returned

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    // na

    // Select based on variable type:  integer, real, or meter
    if (varType == 0) { // Variable not a found variable
        resultVal = 0.0;
    } else if (varType == 1) { // Integer
        if (keyVarIndex > state.dataOutputProcessor->NumOfIVariable) {
            ShowFatalError(state, "GetInternalVariableValue: Integer variable passed index beyond range of array.");
            ShowContinueError(state, format("Index = {} Number of integer variables = {}", keyVarIndex, state.dataOutputProcessor->NumOfIVariable));
        }
        if (keyVarIndex < 1) {
            ShowFatalError(state, format("GetInternalVariableValue: Integer variable passed index <1. Index = {}", keyVarIndex));
        }

        // must use %Which, %Value is always zero if variable is not a requested report variable
        resultVal = double(*state.dataOutputProcessor->IVariableTypes(keyVarIndex).VarPtr.Which);
    } else if (varType == 2) { // real
        if (keyVarIndex > state.dataOutputProcessor->NumOfRVariable) {
            ShowFatalError(state, "GetInternalVariableValue: Real variable passed index beyond range of array.");
            ShowContinueError(state, format("Index = {} Number of real variables = {}", keyVarIndex, state.dataOutputProcessor->NumOfRVariable));
        }
        if (keyVarIndex < 1) {
            ShowFatalError(state, format("GetInternalVariableValue: Integer variable passed index <1. Index = {}", keyVarIndex));
        }

        // must use %Which, %Value is always zero if variable is not a requested report variable
        resultVal = *state.dataOutputProcessor->RVariableTypes(keyVarIndex).VarPtr.Which;
    } else if (varType == 3) { // Meter
        resultVal = GetCurrentMeterValue(state, keyVarIndex);
    } else if (varType == 4) { // Schedule
        resultVal = GetCurrentScheduleValue(state, keyVarIndex);
    } else {
        resultVal = 0.0;
    }

    return resultVal;
}

Real64 GetInternalVariableValueExternalInterface(EnergyPlusData &state, int const varType,    // 1=integer, 2=REAL(r64), 3=meter
                                                 int const keyVarIndex // Array index
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Thierry S. Nouidui
    //       DATE WRITTEN   August 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function returns the last zone-timestep value of the Internal Variable assigned to
    // the varType and keyVarIndex.  Values may be accessed for REAL(r64) and integer
    // report variables and meter variables.  The variable type (varType) may be
    // determined by calling subroutine and GetVariableKeyCountandType.  The
    // index (keyVarIndex) may be determined by calling subroutine GetVariableKeys.

    // METHODOLOGY EMPLOYED:
    // Uses Internal OutputProcessor data structure to return value.

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;
    using ScheduleManager::GetCurrentScheduleValue;

    // Return value
    Real64 resultVal; // value returned

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    // na

    // Select based on variable type:  integer, REAL(r64), or meter
    if (varType == 0) { // Variable not a found variable
        resultVal = 0.0;
    } else if (varType == 1) { // Integer
        if (keyVarIndex > state.dataOutputProcessor->NumOfIVariable) {
            ShowFatalError(state, "GetInternalVariableValueExternalInterface: passed index beyond range of array.");
        }
        if (keyVarIndex < 1) {
            ShowFatalError(state, "GetInternalVariableValueExternalInterface: passed index beyond range of array.");
        }

        // must use %EITSValue, %This is the last-zonetimestep value
        resultVal = double(state.dataOutputProcessor->IVariableTypes(keyVarIndex).VarPtr.EITSValue);
    } else if (varType == 2) { // REAL(r64)
        if (keyVarIndex > state.dataOutputProcessor->NumOfRVariable) {
            ShowFatalError(state, "GetInternalVariableValueExternalInterface: passed index beyond range of array.");
        }
        if (keyVarIndex < 1) {
            ShowFatalError(state, "GetInternalVariableValueExternalInterface: passed index beyond range of array.");
        }

        // must use %EITSValue, %This is the last-zonetimestep value
        resultVal = state.dataOutputProcessor->RVariableTypes(keyVarIndex).VarPtr.EITSValue;
    } else if (varType == 3) { // Meter
        resultVal = GetCurrentMeterValue(state, keyVarIndex);
    } else if (varType == 4) { // Schedule
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
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function counts the number of metered variables associated with the
    // given ComponentType/Name.   This resultant number would then be used to
    // allocate arrays for a call the GetMeteredVariables routine.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;

    // Return value
    int NumVariables;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Loop;

    NumVariables = 0;
    for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
        //    Pos=INDEX(RVariableTypes(Loop)%VarName,':')
        //    IF (ComponentName /= RVariableTypes(Loop)%VarNameUC(1:Pos-1)) CYCLE
        if (ComponentName != state.dataOutputProcessor->RVariableTypes(Loop).KeyNameOnlyUC) continue;
        auto &rVar(state.dataOutputProcessor->RVariableTypes(Loop).VarPtr);
        if (rVar.MeterArrayPtr == 0) {
            continue;
        }
        if (state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).NumOnMeters > 0) {
            ++NumVariables;
        }
    }

    return NumVariables;
}

void GetMeteredVariables(EnergyPlusData &state, std::string const &ComponentType,                      // Given Component Type
                         std::string const &ComponentName,                      // Given Component Name (user defined)
                         Array1D_int &VarIndexes,                               // Variable Numbers
                         Array1D_int &VarTypes,                                 // Variable Types (1=integer, 2=real, 3=meter)
                         Array1D<OutputProcessor::TimeStepType> &TimeStepTypes, // Variable Index Types (1=Zone,2=HVAC)
                         Array1D<OutputProcessor::Unit> &unitsForVar,           // units from enum for each variable
                         std::map<int, DataGlobalConstants::ResourceType> &ResourceTypes,                            // ResourceTypes for each variable
                         Array1D_string &EndUses,                               // EndUses for each variable
                         Array1D_string &Groups,                                // Groups for each variable
                         Array1D_string &Names,                                 // Variable Names for each variable
                         int &NumFound                                          // Number Found
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
    using namespace DataGlobalConstants;
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    int NumVariables;
    int MeterPtr;
    int NumOnMeterPtr;
    int MeterNum;

    NumVariables = 0;

    for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
        //    Pos=INDEX(RVariableTypes(Loop)%VarName,':')
        //    IF (ComponentName /= RVariableTypes(Loop)%VarNameUC(1:Pos-1)) CYCLE
        if (ComponentName != state.dataOutputProcessor->RVariableTypes(Loop).KeyNameOnlyUC) continue;
        auto &rVar(state.dataOutputProcessor->RVariableTypes(Loop).VarPtr);
        if (rVar.MeterArrayPtr == 0) continue;
        NumOnMeterPtr = state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).NumOnMeters;
        MeterPtr = state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).OnMeters(1);
        if (MeterPtr) {
            ++NumVariables;
            VarIndexes(NumVariables) = Loop;
            VarTypes(NumVariables) = 2;
            TimeStepTypes(NumVariables) = state.dataOutputProcessor->RVariableTypes(Loop).timeStepType;
            unitsForVar(NumVariables) = state.dataOutputProcessor->RVariableTypes(Loop).units;

            ResourceTypes.at(NumVariables) = AssignResourceTypeNum(UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->EnergyMeters(MeterPtr).ResourceType));

            Names(NumVariables) = state.dataOutputProcessor->RVariableTypes(Loop).VarNameUC;

            for (MeterNum = 1; MeterNum <= NumOnMeterPtr; ++MeterNum) {
                MeterPtr = state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).OnMeters(MeterNum);
                if (!state.dataOutputProcessor->EnergyMeters(MeterPtr).EndUse.empty()) {
                    EndUses(NumVariables) = UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->EnergyMeters(MeterPtr).EndUse);
                    break;
                }
            }

            for (MeterNum = 1; MeterNum <= NumOnMeterPtr; ++MeterNum) {
                MeterPtr = state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).OnMeters(MeterNum);
                if (!state.dataOutputProcessor->EnergyMeters(MeterPtr).Group.empty()) {
                    Groups(NumVariables) = UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->EnergyMeters(MeterPtr).Group);
                    break;
                }
            }

        } else {
            ShowWarningError(state, "Referenced variable or meter used in the wrong context \"" + ComponentName + "\" of type \"" + ComponentType + "\"");
        }
    }

    NumFound = NumVariables; // Should just return this
}

void GetMeteredVariables(EnergyPlusData &state, std::string const &ComponentType,                      // Given Component Type
                         std::string const &ComponentName,                      // Given Component Name (user defined)
                         Array1D_int &VarIndexes,                               // Variable Numbers
                         Array1D_int &VarTypes,                                 // Variable Types (1=integer, 2=real, 3=meter)
                         Array1D<OutputProcessor::TimeStepType> &TimeStepTypes, // Variable Index Types (1=Zone,2=HVAC)
                         Array1D<OutputProcessor::Unit> &unitsForVar,           // units from enum for each variable
                         std::map<int, DataGlobalConstants::ResourceType> &ResourceTypes,                            // ResourceTypes for each variable
                         Array1D_string &EndUses,                               // EndUses for each variable
                         Array1D_string &Groups,                                // Groups for each variable
                         Array1D_string &Names,                                 // Variable Names for each variable
                         Array1D_int &VarIDs                                    // Variable Report Numbers
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
    using namespace DataGlobalConstants;
    using namespace OutputProcessor;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    int NumVariables;
    int MeterPtr;
    int NumOnMeterPtr;
    int MeterNum;

    NumVariables = 0;

    for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
        //    Pos=INDEX(RVariableTypes(Loop)%VarName,':')
        //    IF (ComponentName /= RVariableTypes(Loop)%VarNameUC(1:Pos-1)) CYCLE
        if (ComponentName != state.dataOutputProcessor->RVariableTypes(Loop).KeyNameOnlyUC) continue;
        auto &rVar(state.dataOutputProcessor->RVariableTypes(Loop).VarPtr);
        if (rVar.MeterArrayPtr == 0) continue;
        NumOnMeterPtr = state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).NumOnMeters;
        MeterPtr = state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).OnMeters(1);
        if (MeterPtr) {
            ++NumVariables;
            VarIndexes(NumVariables) = Loop;
            VarTypes(NumVariables) = 2;
            TimeStepTypes(NumVariables) = state.dataOutputProcessor->RVariableTypes(Loop).timeStepType;
            unitsForVar(NumVariables) = state.dataOutputProcessor->RVariableTypes(Loop).units;

            ResourceTypes.at(NumVariables) = AssignResourceTypeNum(UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->EnergyMeters(MeterPtr).ResourceType));
            Names(NumVariables) = state.dataOutputProcessor->RVariableTypes(Loop).VarNameUC;

            for (MeterNum = 1; MeterNum <= NumOnMeterPtr; ++MeterNum) {
                MeterPtr = state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).OnMeters(MeterNum);
                if (!state.dataOutputProcessor->EnergyMeters(MeterPtr).EndUse.empty()) {
                    EndUses(NumVariables) = UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->EnergyMeters(MeterPtr).EndUse);
                    break;
                }
            }

           for (MeterNum = 1; MeterNum <= NumOnMeterPtr; ++MeterNum) {
                MeterPtr = state.dataOutputProcessor->VarMeterArrays(rVar.MeterArrayPtr).OnMeters(MeterNum);
                if (!state.dataOutputProcessor->EnergyMeters(MeterPtr).Group.empty()) {
                    Groups(NumVariables) = UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->EnergyMeters(MeterPtr).Group);
                    break;
                }
            }

            VarIDs(NumVariables) = rVar.ReportID;

        } else {
            ShowWarningError(state, "Referenced variable or meter used in the wrong context \"" + ComponentName + "\" of type \"" + ComponentType + "\"");
        }
    }

}

void GetVariableKeyCountandType(EnergyPlusData &state,
                                std::string const &varName,                 // Standard variable name
                                int &numKeys,                               // Number of keys found
                                int &varType,                               // 0=not found, 1=integer, 2=real, 3=meter
                                OutputProcessor::StoreType &varAvgSum,      // Variable  is Averaged=1 or Summed=2
                                OutputProcessor::TimeStepType &varStepType, // Variable time step is Zone=1 or HVAC=2
                                OutputProcessor::Unit &varUnits             // Units enumeration
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael J. Witte
    //       DATE WRITTEN   August 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

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
    // varType is assigned as follows:
    //       0 = not found
    //       1 = integer
    //       2 = real
    //       3 = meter
    //       4 = schedule
    //  varAvgSum is assigned as follows:
    //       1 = averaged
    //       2 = summed
    //  varStepType is assigned as follows:
    //       1 = zone time step
    //       2 = HVAC time step

    // Using/Aliasing
    using namespace OutputProcessor;
    using ScheduleManager::GetScheduleIndex;
    using ScheduleManager::GetScheduleType;
    using SortAndStringUtilities::SetupAndSort;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    //////////// hoisted into namespace ///////////////////////////////////////////////
    // static bool InitFlag( true ); // for initting the keyVarIndexes array
    ////////////////////////////////////////////////////////////////////////////////////
    int Loop; // Loop counters
    int Loop2;
    std::string::size_type Position; // Starting point of search string
    int VFound;                      // Found integer/real variable attributes
    bool Found;                      // True if varName is found
    bool Duplicate;                  // True if keyname is a duplicate
    std::string VarKeyPlusName;      // Full variable name including keyname and units
    std::string varNameUpper;        // varName pushed to all upper case

    // INITIALIZATIONS
    if (state.dataOutputProcessor->InitFlag) {
        state.dataOutputProcessor->curKeyVarIndexLimit = 1000;
        state.dataOutputProcessor->keyVarIndexes.allocate(state.dataOutputProcessor->curKeyVarIndexLimit);
        state.dataOutputProcessor->numVarNames = state.dataOutputProcessor->NumVariablesForOutput;
        state.dataOutputProcessor->varNames.allocate(state.dataOutputProcessor->numVarNames);
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumVariablesForOutput; ++Loop) {
            state.dataOutputProcessor->varNames(Loop) = UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->DDVariableTypes(Loop).VarNameOnly);
        }
        state.dataOutputProcessor->ivarNames.allocate(state.dataOutputProcessor->numVarNames);
        SetupAndSort(state.dataOutputProcessor->varNames, state.dataOutputProcessor->ivarNames);
        state.dataOutputProcessor->InitFlag = false;
    }

    if (state.dataOutputProcessor->numVarNames != state.dataOutputProcessor->NumVariablesForOutput) {
        state.dataOutputProcessor->numVarNames = state.dataOutputProcessor->NumVariablesForOutput;
        state.dataOutputProcessor->varNames.allocate(state.dataOutputProcessor->numVarNames);
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumVariablesForOutput; ++Loop) {
            state.dataOutputProcessor->varNames(Loop) = UtilityRoutines::MakeUPPERCase(state.dataOutputProcessor->DDVariableTypes(Loop).VarNameOnly);
        }
        state.dataOutputProcessor->ivarNames.allocate(state.dataOutputProcessor->numVarNames);
        SetupAndSort(state.dataOutputProcessor->varNames, state.dataOutputProcessor->ivarNames);
    }

    state.dataOutputProcessor->keyVarIndexes = 0;
    varType = VarType_NotFound;
    numKeys = 0;
    varAvgSum = StoreType::Averaged;
    varStepType = TimeStepType::TimeStepZone;
    varUnits = OutputProcessor::Unit::None;
    Found = false;
    Duplicate = false;
    varNameUpper = varName;

    // Search Variable List First
    VFound = UtilityRoutines::FindItemInSortedList(varNameUpper, state.dataOutputProcessor->varNames, state.dataOutputProcessor->numVarNames);
    if (VFound != 0) {
        varType = state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->ivarNames(VFound)).VariableType;
    }

    if (varType == VarType_Integer) {
        // Search Integer Variables
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
            VarKeyPlusName = state.dataOutputProcessor->IVariableTypes(Loop).VarNameUC;
            Position = index(VarKeyPlusName, ':' + varNameUpper, true);
            if (Position != std::string::npos) {
                if (VarKeyPlusName.substr(Position + 1) == varNameUpper) {
                    Found = true;
                    varType = VarType_Integer;
                    Duplicate = false;
                    // Check if duplicate - duplicates happen if the same report variable/key name
                    // combination is requested more than once in the idf at different reporting
                    // frequencies
                    for (Loop2 = 1; Loop2 <= numKeys; ++Loop2) {
                        if (VarKeyPlusName == state.dataOutputProcessor->IVariableTypes(state.dataOutputProcessor->keyVarIndexes(Loop2)).VarNameUC) Duplicate = true;
                    }
                    if (!Duplicate) {
                        ++numKeys;
                        if (numKeys > state.dataOutputProcessor->curKeyVarIndexLimit) {
                            state.dataOutputProcessor->keyVarIndexes.redimension(state.dataOutputProcessor->curKeyVarIndexLimit += 500, 0);
                        }
                        state.dataOutputProcessor->keyVarIndexes(numKeys) = Loop;
                        varAvgSum = state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->ivarNames(VFound)).storeType;
                        varStepType = state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->ivarNames(VFound)).timeStepType;
                        varUnits = state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->ivarNames(VFound)).units;
                    }
                }
            }
        }
    } else if (varType == VarType_Real) {
        // Search real Variables Next
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
            if (state.dataOutputProcessor->RVariableTypes(Loop).VarNameOnlyUC == varNameUpper) {
                Found = true;
                varType = VarType_Real;
                Duplicate = false;
                // Check if duplicate - duplicates happen if the same report variable/key name
                // combination is requested more than once in the idf at different reporting
                // frequencies
                VarKeyPlusName = state.dataOutputProcessor->RVariableTypes(Loop).VarNameUC;
                for (Loop2 = 1; Loop2 <= numKeys; ++Loop2) {
                    if (VarKeyPlusName == state.dataOutputProcessor->RVariableTypes(state.dataOutputProcessor->keyVarIndexes(Loop2)).VarNameUC) Duplicate = true;
                }
                if (!Duplicate) {
                    ++numKeys;
                    if (numKeys > state.dataOutputProcessor->curKeyVarIndexLimit) {
                        state.dataOutputProcessor->keyVarIndexes.redimension(state.dataOutputProcessor->curKeyVarIndexLimit += 500, 0);
                    }
                    state.dataOutputProcessor->keyVarIndexes(numKeys) = Loop;
                    varAvgSum = state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->ivarNames(VFound)).storeType;
                    varStepType = state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->ivarNames(VFound)).timeStepType;
                    varUnits = state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->ivarNames(VFound)).units;
                }
            }
        }
    }

    // Search Meters if not found in integers or reals
    // Use the GetMeterIndex function
    // Meters do not have keys, so only one will be found
    if (!Found) {
        state.dataOutputProcessor->keyVarIndexes(1) = GetMeterIndex(state, varName);
        if (state.dataOutputProcessor->keyVarIndexes(1) > 0) {
            Found = true;
            numKeys = 1;
            varType = VarType_Meter;
            varUnits = state.dataOutputProcessor->EnergyMeters(state.dataOutputProcessor->keyVarIndexes(1)).Units;
            varAvgSum = StoreType::Summed;
            varStepType = TimeStepType::TimeStepZone;
        }
    }

    // Search schedules if not found in integers, reals, or meters
    // Use the GetScheduleIndex function
    // Schedules do not have keys, so only one will be found
    if (!Found) {
        state.dataOutputProcessor->keyVarIndexes(1) = GetScheduleIndex(state, varName);
        if (state.dataOutputProcessor->keyVarIndexes(1) > 0) {
            Found = true;
            numKeys = 1;
            varType = VarType_Schedule;
            varUnits = unitStringToEnum(GetScheduleType(state, state.dataOutputProcessor->keyVarIndexes(1)));
            varAvgSum = StoreType::Averaged;
            varStepType = TimeStepType::TimeStepZone;
        }
    }
}

void GetVariableKeys(EnergyPlusData &state,
                     std::string const &varName, // Standard variable name
                     int const varType,          // 1=integer, 2=real, 3=meter
                     Array1D_string &keyNames,   // Specific key name
                     Array1D_int &keyVarIndexes  // Array index for
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael J. Witte
    //       DATE WRITTEN   August 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

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
    int Loop; // Loop counters
    int Loop2;
    std::string::size_type Position; // Starting point of search string
    bool Duplicate;                  // True if keyname is a duplicate
    int maxKeyNames;                 // Max allowable # of key names=size of keyNames array
    int maxkeyVarIndexes;            // Max allowable # of key indexes=size of keyVarIndexes array
    int numKeys;                     // Number of keys found
    std::string VarKeyPlusName;      // Full variable name including keyname and units
    std::string varNameUpper;        // varName pushed to all upper case

    // INITIALIZATIONS
    keyNames = "";
    keyVarIndexes = 0;
    numKeys = 0;
    Duplicate = false;
    maxKeyNames = size(keyNames);
    maxkeyVarIndexes = size(keyVarIndexes);
    varNameUpper = UtilityRoutines::MakeUPPERCase(varName);

    // Select based on variable type:  integer, real, or meter
    if (varType == VarType_Integer) { // Integer
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfIVariable; ++Loop) {
            VarKeyPlusName = state.dataOutputProcessor->IVariableTypes(Loop).VarNameUC;
            Position = index(VarKeyPlusName, ':' + varNameUpper, true);
            if (Position != std::string::npos) {
                if (VarKeyPlusName.substr(Position + 1) == varNameUpper) {
                    Duplicate = false;
                    // Check if duplicate - duplicates happen if the same report variable/key name
                    // combination is requested more than once in the idf at different reporting
                    // frequencies
                    for (Loop2 = 1; Loop2 <= numKeys; ++Loop2) {
                        if (VarKeyPlusName == state.dataOutputProcessor->IVariableTypes(keyVarIndexes(Loop2)).VarNameUC) Duplicate = true;
                    }
                    if (!Duplicate) {
                        ++numKeys;
                        if ((numKeys > maxKeyNames) || (numKeys > maxkeyVarIndexes)) {
                            ShowFatalError(state, "Invalid array size in GetVariableKeys");
                        }
                        keyNames(numKeys) = state.dataOutputProcessor->IVariableTypes(Loop).VarNameUC.substr(0, Position);
                        keyVarIndexes(numKeys) = Loop;
                    }
                }
            }
        }
    } else if (varType == VarType_Real) { // Real
        for (Loop = 1; Loop <= state.dataOutputProcessor->NumOfRVariable; ++Loop) {
            if (state.dataOutputProcessor->RVariableTypes(Loop).VarNameOnlyUC == varNameUpper) {
                Duplicate = false;
                // Check if duplicate - duplicates happen if the same report variable/key name
                // combination is requested more than once in the idf at different reporting
                // frequencies
                VarKeyPlusName = state.dataOutputProcessor->RVariableTypes(Loop).VarNameUC;
                for (Loop2 = 1; Loop2 <= numKeys; ++Loop2) {
                    if (VarKeyPlusName == state.dataOutputProcessor->RVariableTypes(keyVarIndexes(Loop2)).VarNameUC) Duplicate = true;
                }
                if (!Duplicate) {
                    ++numKeys;
                    if ((numKeys > maxKeyNames) || (numKeys > maxkeyVarIndexes)) {
                        ShowFatalError(state, "Invalid array size in GetVariableKeys");
                    }
                    keyNames(numKeys) = state.dataOutputProcessor->RVariableTypes(Loop).KeyNameOnlyUC;
                    keyVarIndexes(numKeys) = Loop;
                }
            }
        }
    } else if (varType == VarType_Meter) { // Meter
        numKeys = 1;
        if ((numKeys > maxKeyNames) || (numKeys > maxkeyVarIndexes)) {
            ShowFatalError(state, "Invalid array size in GetVariableKeys");
        }
        keyNames(1) = "Meter";
        keyVarIndexes(1) = GetMeterIndex(state, varName);
    } else if (varType == VarType_Schedule) { // Schedule
        numKeys = 1;
        if ((numKeys > maxKeyNames) || (numKeys > maxkeyVarIndexes)) {
            ShowFatalError(state, "Invalid array size in GetVariableKeys");
        }
        keyNames(1) = "Environment";
        keyVarIndexes(1) = GetScheduleIndex(state, varName);
    } else {
        // do nothing
    }
}

bool ReportingThisVariable(EnergyPlusData &state, std::string const &RepVarName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function scans the report variables and reports back
    // if user has requested this variable be reported.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;

    // Return value
    bool BeingReported;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Found;

    BeingReported = false;
    Found = UtilityRoutines::FindItem(RepVarName, state.dataOutputProcessor->ReqRepVars, &ReqReportVariables::VarName);
    if (Found > 0) {
        BeingReported = true;
    }

    if (!BeingReported) { // check meter names too
        Found = UtilityRoutines::FindItem(RepVarName, state.dataOutputProcessor->EnergyMeters);
        if (Found > 0) {
            if (state.dataOutputProcessor->EnergyMeters(Found).RptTS || state.dataOutputProcessor->EnergyMeters(Found).RptHR || state.dataOutputProcessor->EnergyMeters(Found).RptDY || state.dataOutputProcessor->EnergyMeters(Found).RptMN ||
                state.dataOutputProcessor->EnergyMeters(Found).RptSM || state.dataOutputProcessor->EnergyMeters(Found).RptTSFO || state.dataOutputProcessor->EnergyMeters(Found).RptHRFO || state.dataOutputProcessor->EnergyMeters(Found).RptDYFO ||
                state.dataOutputProcessor->EnergyMeters(Found).RptMNFO || state.dataOutputProcessor->EnergyMeters(Found).RptSMFO || state.dataOutputProcessor->EnergyMeters(Found).RptAccTS || state.dataOutputProcessor->EnergyMeters(Found).RptAccHR ||
                state.dataOutputProcessor->EnergyMeters(Found).RptAccDY || state.dataOutputProcessor->EnergyMeters(Found).RptAccMN || state.dataOutputProcessor->EnergyMeters(Found).RptAccSM || state.dataOutputProcessor->EnergyMeters(Found).RptAccTSFO ||
                state.dataOutputProcessor->EnergyMeters(Found).RptAccHRFO || state.dataOutputProcessor->EnergyMeters(Found).RptAccDYFO || state.dataOutputProcessor->EnergyMeters(Found).RptAccMNFO ||
                state.dataOutputProcessor->EnergyMeters(Found).RptAccSMFO) {
                BeingReported = true;
            }
        }
    }

    return BeingReported;
}

void InitPollutionMeterReporting(EnergyPlusData &state, std::string const &ReportFreqName)
{

    // SUBROUTINE INFORMATION:Richard Liesen
    //       DATE WRITTEN   July 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

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
    // SUBROUTINE PARAMETER DEFINITIONS:
    //             Now for the Pollution Meters
    static Array1D_string const PollutionMeters({1, 29},
                                                {"Electricity:Facility",
                                                 "Diesel:Facility",
                                                 "DistrictCooling:Facility",
                                                 "DistrictHeating:Facility",
                                                 "NaturalGas:Facility",
                                                 "GASOLINE:Facility",
                                                 "COAL:Facility",
                                                 "FuelOilNo1:Facility",
                                                 "FuelOilNo2:Facility",
                                                 "Propane:Facility",
                                                 "ElectricityProduced:Facility",
                                                 "Steam:Facility",
                                                 "CO2:Facility",
                                                 "CO:Facility",
                                                 "CH4:Facility",
                                                 "NOx:Facility",
                                                 "N2O:Facility",
                                                 "SO2:Facility",
                                                 "PM:Facility",
                                                 "PM10:Facility",
                                                 "PM2.5:Facility",
                                                 "NH3:Facility",
                                                 "NMVOC:Facility",
                                                 "Hg:Facility",
                                                 "Pb:Facility",
                                                 "WaterEnvironmentalFactors:Facility",
                                                 "Nuclear High:Facility",
                                                 "Nuclear Low:Facility",
                                                 "Carbon Equivalent:Facility"});

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    int NumReqMeters;
    int Meter;
    ReportingFrequency ReportFreq(determineFrequency(state, ReportFreqName));

    int indexGroupKey;
    std::string indexGroup;

    NumReqMeters = 29;

    for (Loop = 1; Loop <= NumReqMeters; ++Loop) {

        Meter = UtilityRoutines::FindItem(PollutionMeters(Loop), state.dataOutputProcessor->EnergyMeters);
        if (Meter > 0) { // All the active meters for this run are set, but all are still searched for.

            indexGroupKey = DetermineIndexGroupKeyFromMeterName(state.dataOutputProcessor->EnergyMeters(Meter).Name);
            indexGroup = DetermineIndexGroupFromMeterGroup(state.dataOutputProcessor->EnergyMeters(Meter));
            // All of the specified meters are checked and the headers printed to the meter file if this
            //  has not been done previously
            if (ReportFreq == ReportingFrequency::TimeStep) {
                if (state.dataOutputProcessor->EnergyMeters(Meter).RptTS) {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptTS = true;
                } else {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptTS = true;
                    WriteMeterDictionaryItem(state,
                                             ReportFreq,
                                             StoreType::Summed,
                                             state.dataOutputProcessor->EnergyMeters(Meter).TSRptNum,
                                             indexGroupKey,
                                             indexGroup,
                                             state.dataOutputProcessor->EnergyMeters(Meter).TSRptNumChr,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Name,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Units,
                                             false,
                                             false);
                }
            } else if (ReportFreq == ReportingFrequency::Hourly) {
                if (state.dataOutputProcessor->EnergyMeters(Meter).RptHR) {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptHR = true;
                    state.dataOutputProcessor->TrackingHourlyVariables = true;
                } else {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptHR = true;
                    state.dataOutputProcessor->TrackingHourlyVariables = true;
                    WriteMeterDictionaryItem(state,
                                             ReportFreq,
                                             StoreType::Summed,
                                             state.dataOutputProcessor->EnergyMeters(Meter).HRRptNum,
                                             indexGroupKey,
                                             indexGroup,
                                             state.dataOutputProcessor->EnergyMeters(Meter).HRRptNumChr,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Name,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Units,
                                             false,
                                             false);
                }
            } else if (ReportFreq == ReportingFrequency::Daily) {
                if (state.dataOutputProcessor->EnergyMeters(Meter).RptDY) {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptDY = true;
                    state.dataOutputProcessor->TrackingDailyVariables = true;
                } else {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptDY = true;
                    state.dataOutputProcessor->TrackingDailyVariables = true;
                    WriteMeterDictionaryItem(state,
                                             ReportFreq,
                                             StoreType::Summed,
                                             state.dataOutputProcessor->EnergyMeters(Meter).DYRptNum,
                                             indexGroupKey,
                                             indexGroup,
                                             state.dataOutputProcessor->EnergyMeters(Meter).DYRptNumChr,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Name,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Units,
                                             false,
                                             false);
                }
            } else if (ReportFreq == ReportingFrequency::Monthly) {
                if (state.dataOutputProcessor->EnergyMeters(Meter).RptMN) {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptMN = true;
                    state.dataOutputProcessor->TrackingMonthlyVariables = true;
                } else {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptMN = true;
                    state.dataOutputProcessor->TrackingMonthlyVariables = true;
                    WriteMeterDictionaryItem(state,
                                             ReportFreq,
                                             StoreType::Summed,
                                             state.dataOutputProcessor->EnergyMeters(Meter).MNRptNum,
                                             indexGroupKey,
                                             indexGroup,
                                             state.dataOutputProcessor->EnergyMeters(Meter).MNRptNumChr,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Name,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Units,
                                             false,
                                             false);
                }
            } else if (ReportFreq == ReportingFrequency::Yearly) {
                if (state.dataOutputProcessor->EnergyMeters(Meter).RptYR) {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptYR = true;
                    state.dataOutputProcessor->TrackingYearlyVariables = true;
                } else {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptYR = true;
                    state.dataOutputProcessor->TrackingMonthlyVariables = true;
                    WriteMeterDictionaryItem(state,
                                             ReportFreq,
                                             StoreType::Summed,
                                             state.dataOutputProcessor->EnergyMeters(Meter).YRRptNum,
                                             indexGroupKey,
                                             indexGroup,
                                             state.dataOutputProcessor->EnergyMeters(Meter).YRRptNumChr,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Name,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Units,
                                             false,
                                             false);
                }
            } else if (ReportFreq == ReportingFrequency::Simulation) {
                if (state.dataOutputProcessor->EnergyMeters(Meter).RptSM) {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptSM = true;
                    state.dataOutputProcessor->TrackingRunPeriodVariables = true;
                } else {
                    state.dataOutputProcessor->EnergyMeters(Meter).RptSM = true;
                    state.dataOutputProcessor->TrackingRunPeriodVariables = true;
                    WriteMeterDictionaryItem(state,
                                             ReportFreq,
                                             StoreType::Summed,
                                             state.dataOutputProcessor->EnergyMeters(Meter).SMRptNum,
                                             indexGroupKey,
                                             indexGroup,
                                             state.dataOutputProcessor->EnergyMeters(Meter).SMRptNumChr,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Name,
                                             state.dataOutputProcessor->EnergyMeters(Meter).Units,
                                             false,
                                             false);
                }
            } else {
            }
        }
    }
}

void ProduceRDDMDD(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // provide a single call for writing out the Report Data Dictionary and Meter Data Dictionary.

    // Using/Aliasing
    using DataStringGlobals::IDDVerString;
    using DataStringGlobals::VerString;
    using namespace OutputProcessor;
    using General::ScanForReports;
    using SortAndStringUtilities::SetupAndSort;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string VarOption1;
    std::string VarOption2;
    bool DoReport;
    int Item;
    bool SortByName;
    int ItemPtr;

    struct VariableTypes
    {
        // Members
        int RealIntegerType; // Real= 1, Integer=2
        int VarPtr;          // pointer to real/integer VariableTypes structures
        int TimeStepType;
        int StoreType;
        std::string UnitsString;

        // Default Constructor
        VariableTypes() : RealIntegerType(0), VarPtr(0), TimeStepType(0), StoreType(0)
        {
        }
    };

    //  See if Report Variables should be turned on

    SortByName = false;
    ScanForReports(state, "VariableDictionary", DoReport, _, VarOption1, VarOption2);
    //  IF (.not. DoReport) RETURN

    if (DoReport) {
        state.dataOutputProcessor->ProduceReportVDD = iReportVDD::Yes;
        if (VarOption1 == std::string("IDF")) {
            state.dataOutputProcessor->ProduceReportVDD = iReportVDD::IDF;
        }
        if (!VarOption2.empty()) {
            if (UtilityRoutines::SameString(VarOption2, "Name") || UtilityRoutines::SameString(VarOption2, "AscendingName")) {
                SortByName = true;
            }
        }
    }

    state.files.rdd.ensure_open(state, "ProduceRDDMDD", state.files.outputControl.rdd);
    state.files.mdd.ensure_open(state, "ProduceRDDMDD", state.files.outputControl.mdd);
    if (state.dataOutputProcessor->ProduceReportVDD == iReportVDD::Yes) {
        print(state.files.rdd, "Program Version,{},{}{}", VerString, IDDVerString, '\n');
        print(state.files.rdd, "Var Type (reported time step),Var Report Type,Variable Name [Units]{}", '\n');

        print(state.files.mdd, "Program Version,{},{}{}", VerString, IDDVerString, '\n');
        print(state.files.mdd, "Var Type (reported time step),Var Report Type,Variable Name [Units]{}", '\n');
    } else if (state.dataOutputProcessor->ProduceReportVDD == iReportVDD::IDF) {
        print(state.files.rdd, "! Program Version,{},{}{}", VerString, IDDVerString, '\n');
        print(state.files.rdd, "! Output:Variable Objects (applicable to this run){}", '\n');

        print(state.files.mdd, "! Program Version,{},{}{}", VerString, IDDVerString, '\n');
        print(state.files.mdd, "! Output:Meter Objects (applicable to this run){}", '\n');
    }

    Array1D_string VariableNames(state.dataOutputProcessor->NumVariablesForOutput);
    for (int i = 1; i <= state.dataOutputProcessor->NumVariablesForOutput; ++i)
        VariableNames(i) = state.dataOutputProcessor->DDVariableTypes(i).VarNameOnly;
    Array1D_int iVariableNames(state.dataOutputProcessor->NumVariablesForOutput);

    if (SortByName) {
        SetupAndSort(VariableNames, iVariableNames);
    } else {
        for (Item = 1; Item <= state.dataOutputProcessor->NumVariablesForOutput; ++Item) {
            iVariableNames(Item) = Item;
        }
    }

    for (Item = 1; Item <= state.dataOutputProcessor->NumVariablesForOutput; ++Item) {
        if (state.dataOutputProcessor->ProduceReportVDD == iReportVDD::Yes) {
            ItemPtr = iVariableNames(Item);
            if (!state.dataOutputProcessor->DDVariableTypes(ItemPtr).ReportedOnDDFile) {
                print(state.files.rdd, "{},{},{}{}{}", StandardTimeStepTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).timeStepType), standardVariableTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).storeType), VariableNames(Item), unitStringFromDDitem(state, ItemPtr), '\n');
                ResultsFramework::resultsFramework->RDD.push_back(StandardTimeStepTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).timeStepType) + "," +
                                                                  standardVariableTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).storeType) + "," +
                                                                  VariableNames(Item) + unitStringFromDDitem(state, ItemPtr));
                state.dataOutputProcessor->DDVariableTypes(ItemPtr).ReportedOnDDFile = true;
                while (state.dataOutputProcessor->DDVariableTypes(ItemPtr).Next != 0) {
                    if (SortByName) {
                        ++ItemPtr;
                    } else {
                        ItemPtr = state.dataOutputProcessor->DDVariableTypes(ItemPtr).Next;
                    }
                    print(state.files.rdd, "{},{},{}{}{}", StandardTimeStepTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).timeStepType), standardVariableTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).storeType), VariableNames(Item), unitStringFromDDitem(state, ItemPtr), '\n');
                    ResultsFramework::resultsFramework->RDD.push_back(StandardTimeStepTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).timeStepType) + "," +
                                                                      standardVariableTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).storeType) + "," +
                                                                      VariableNames(Item) + unitStringFromDDitem(state, ItemPtr));
                    state.dataOutputProcessor->DDVariableTypes(ItemPtr).ReportedOnDDFile = true;
                }
            }
        } else if (state.dataOutputProcessor->ProduceReportVDD == iReportVDD::IDF) {
            ItemPtr = iVariableNames(Item);
            if (!state.dataOutputProcessor->DDVariableTypes(ItemPtr).ReportedOnDDFile) {
                print(state.files.rdd, "Output:Variable,*,{},hourly; !- {} {}{}{}", VariableNames(Item), StandardTimeStepTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).timeStepType), standardVariableTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).storeType), unitStringFromDDitem(state, ItemPtr), '\n');
                ResultsFramework::resultsFramework->RDD.push_back(StandardTimeStepTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).timeStepType) + "," +
                                                                  standardVariableTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).storeType) + "," +
                                                                  VariableNames(Item) + unitStringFromDDitem(state, ItemPtr));
                state.dataOutputProcessor->DDVariableTypes(ItemPtr).ReportedOnDDFile = true;
                while (state.dataOutputProcessor->DDVariableTypes(ItemPtr).Next != 0) {
                    if (SortByName) {
                        ++ItemPtr;
                    } else {
                        ItemPtr = state.dataOutputProcessor->DDVariableTypes(ItemPtr).Next;
                    }
                    print(state.files.rdd, "Output:Variable,*,{},hourly; !- {} {}{}{}", VariableNames(Item), StandardTimeStepTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).timeStepType), standardVariableTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).storeType), unitStringFromDDitem(state, ItemPtr), '\n');
                    ResultsFramework::resultsFramework->RDD.push_back(StandardTimeStepTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).timeStepType) + "," +
                                                                      standardVariableTypeKey(state.dataOutputProcessor->DDVariableTypes(ItemPtr).storeType) + "," +
                                                                      VariableNames(Item) + unitStringFromDDitem(state, ItemPtr));
                    state.dataOutputProcessor->DDVariableTypes(ItemPtr).ReportedOnDDFile = true;
                }
            }
        }
    }
    state.files.rdd.close();

    //  Now EnergyMeter variables
    VariableNames.allocate(state.dataOutputProcessor->NumEnergyMeters);
    iVariableNames.allocate(state.dataOutputProcessor->NumEnergyMeters);
    if (SortByName) {
        for (Item = 1; Item <= state.dataOutputProcessor->NumEnergyMeters; ++Item) {
            VariableNames(Item) = state.dataOutputProcessor->EnergyMeters(Item).Name;
        }
        SetupAndSort(VariableNames, iVariableNames);
    } else {
        for (Item = 1; Item <= state.dataOutputProcessor->NumEnergyMeters; ++Item) {
            VariableNames(Item) = state.dataOutputProcessor->EnergyMeters(Item).Name;
            iVariableNames(Item) = Item;
        }
    }

    for (Item = 1; Item <= state.dataOutputProcessor->NumEnergyMeters; ++Item) {
        ItemPtr = iVariableNames(Item);
        if (state.dataOutputProcessor->ProduceReportVDD == iReportVDD::Yes) {
            print(state.files.mdd, "Zone,Meter,{}{}{}", state.dataOutputProcessor->EnergyMeters(ItemPtr).Name, unitEnumToStringBrackets(state.dataOutputProcessor->EnergyMeters(ItemPtr).Units), '\n');
            ResultsFramework::resultsFramework->MDD.push_back("Zone,Meter," + state.dataOutputProcessor->EnergyMeters(ItemPtr).Name +
                                                              unitEnumToStringBrackets(state.dataOutputProcessor->EnergyMeters(ItemPtr).Units));
        } else if (state.dataOutputProcessor->ProduceReportVDD == iReportVDD::IDF) {
            print(state.files.mdd, "Output:Meter,{},hourly; !-{}{}", state.dataOutputProcessor->EnergyMeters(ItemPtr).Name, unitEnumToStringBrackets(state.dataOutputProcessor->EnergyMeters(ItemPtr).Units), '\n');
            ResultsFramework::resultsFramework->MDD.push_back("Output:Meter," + state.dataOutputProcessor->EnergyMeters(ItemPtr).Name +
                                                              unitEnumToStringBrackets(state.dataOutputProcessor->EnergyMeters(ItemPtr).Units));
            print(state.files.mdd, "Output:Meter:Cumulative,{},hourly; !-{}{}", state.dataOutputProcessor->EnergyMeters(ItemPtr).Name, unitEnumToStringBrackets(state.dataOutputProcessor->EnergyMeters(ItemPtr).Units), '\n');
            ResultsFramework::resultsFramework->MDD.push_back("Output:Meter:Cumulative," + state.dataOutputProcessor->EnergyMeters(ItemPtr).Name +
                                                              unitEnumToStringBrackets(state.dataOutputProcessor->EnergyMeters(ItemPtr).Units));
        }
    }
    state.files.mdd.close();
}

void AddToOutputVariableList(EnergyPlusData &state,
                             std::string const &VarName, // Variable Name
                             OutputProcessor::TimeStepType const TimeStepType,
                             OutputProcessor::StoreType const StateType,
                             int const VariableType,
                             OutputProcessor::Unit const unitsForVar,
                             Optional_string_const customUnitName // the custom name for the units from EMS definition of units
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   August 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine maintains a unique list of Output Variables for the
    // Variable Dictionary output.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace OutputProcessor;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int dup = 0; // for duplicate variable name
    if (state.dataOutputProcessor->NumVariablesForOutput > 0) {
        dup = UtilityRoutines::FindItemInList(VarName, state.dataOutputProcessor->DDVariableTypes, &VariableTypeForDDOutput::VarNameOnly, state.dataOutputProcessor->NumVariablesForOutput);
    } else {
        state.dataOutputProcessor->DDVariableTypes.allocate(LVarAllocInc);
        state.dataOutputProcessor->MaxVariablesForOutput = LVarAllocInc;
    }
    if (dup == 0) {
        ++state.dataOutputProcessor->NumVariablesForOutput;
        if (state.dataOutputProcessor->NumVariablesForOutput > state.dataOutputProcessor->MaxVariablesForOutput) {
            state.dataOutputProcessor->DDVariableTypes.redimension(state.dataOutputProcessor->MaxVariablesForOutput += LVarAllocInc);
        }
        state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).timeStepType = TimeStepType;
        state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).storeType = StateType;
        state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).VariableType = VariableType;
        state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).VarNameOnly = VarName;
        state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).units = unitsForVar;
        if (present(customUnitName) && unitsForVar == OutputProcessor::Unit::customEMS) {
            state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).unitNameCustomEMS = customUnitName;
        }
    } else if (unitsForVar != state.dataOutputProcessor->DDVariableTypes(dup).units) { // not the same as first units
        int dup2 = 0;                                       // for duplicate variable name
        while (state.dataOutputProcessor->DDVariableTypes(dup).Next != 0) {
            if (unitsForVar != state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->DDVariableTypes(dup).Next).units) {
                dup = state.dataOutputProcessor->DDVariableTypes(dup).Next;
                continue;
            }
            dup2 = state.dataOutputProcessor->DDVariableTypes(dup).Next;
            break;
        }
        if (dup2 == 0) {
            ++state.dataOutputProcessor->NumVariablesForOutput;
            if (state.dataOutputProcessor->NumVariablesForOutput > state.dataOutputProcessor->MaxVariablesForOutput) {
                state.dataOutputProcessor->DDVariableTypes.redimension(state.dataOutputProcessor->MaxVariablesForOutput += LVarAllocInc);
            }
            state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).timeStepType = TimeStepType;
            state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).storeType = StateType;
            state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).VariableType = VariableType;
            state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).VarNameOnly = VarName;
            state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).units = unitsForVar;
            if (present(customUnitName) && unitsForVar == OutputProcessor::Unit::customEMS) {
                state.dataOutputProcessor->DDVariableTypes(state.dataOutputProcessor->NumVariablesForOutput).unitNameCustomEMS = customUnitName;
            }
            state.dataOutputProcessor->DDVariableTypes(dup).Next = state.dataOutputProcessor->NumVariablesForOutput;
        }
    }
}

int initErrorFile(EnergyPlusData &state)
{
    state.files.err_stream = std::unique_ptr<std::ostream>(new std::ofstream(state.files.outputErrFileName));
    if (state.files.err_stream->bad()) {
        DisplayString(state, "ERROR: Could not open file " + state.files.outputErrFileName + " for output (write).");
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}

} // namespace EnergyPlus
