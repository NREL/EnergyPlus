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

#ifndef DataOutputs_hh_INCLUDED
#define DataOutputs_hh_INCLUDED

// C++ Headers
#include <cstddef>
#include <unordered_map>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include "re2/re2.h"
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataOutputs {

    int constexpr NumMonthlyReports(63);

    extern Array1D_string const MonthlyNamedReports;

    struct OutputReportingVariables
    {
        OutputReportingVariables(EnergyPlusData &state, std::string const &KeyValue, std::string const &VariableName);

        std::string const key;
        std::string const variableName;
        bool is_simple_string = true;
        std::shared_ptr<RE2> pattern = nullptr;
        std::shared_ptr<RE2> case_insensitive_pattern = nullptr;
    };

    // Check if a KeyValue/VariableName is inside the map OutputVariablesForSimulation
    bool FindItemInVariableList(EnergyPlusData &state, std::string const &KeyedValue, std::string const &VariableName);

} // namespace DataOutputs

struct OutputsData : BaseGlobalStruct
{

    int MaxConsideredOutputVariables = 0; // Max Array size for OutputVariable pre-scanned
    int NumConsideredOutputVariables = 0; // Number of variables - pre-scanned, allowed for output
    int iNumberOfRecords;                 // Number of records in input
    int iNumberOfDefaultedFields;         // number of defaulted fields
    int iTotalFieldsWithDefaults;         // number of fields that can be defaulted
    int iNumberOfAutoSizedFields;         // number of autosized fields
    int iTotalAutoSizableFields;          // number of fields that can be autosized
    int iNumberOfAutoCalcedFields;        // number of autocalculated fields
    int iTotalAutoCalculatableFields;     // number of fields that can be autocalculated
    std::unordered_map<std::string,
                       std::unordered_map<std::string,
                                          DataOutputs::OutputReportingVariables,
                                          UtilityRoutines::case_insensitive_hasher,
                                          UtilityRoutines::case_insensitive_comparator>,
                       UtilityRoutines::case_insensitive_hasher,
                       UtilityRoutines::case_insensitive_comparator>
        OutputVariablesForSimulation;

    void clear_state() override
    {
        MaxConsideredOutputVariables = 0;
        NumConsideredOutputVariables = 0;
        iNumberOfRecords = int();
        iNumberOfDefaultedFields = int();
        iTotalFieldsWithDefaults = int();
        iNumberOfAutoSizedFields = int();
        iTotalAutoSizableFields = int();
        iNumberOfAutoCalcedFields = int();
        iTotalAutoCalculatableFields = int();
        OutputVariablesForSimulation.clear();
    }
};

} // namespace EnergyPlus

#endif
