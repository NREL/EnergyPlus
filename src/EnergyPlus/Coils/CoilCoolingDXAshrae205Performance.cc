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

#include <EnergyPlus/Coils/CoilCoolingDXAshrae205Performance.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;

CoilCoolingDX205Performance::CoilCoolingDX205Performance(EnergyPlus::EnergyPlusData &state, const std::string &name_to_find)
{
    using namespace tk205;
    RSInstanceFactory::register_factory("RS0004", std::make_shared<RS0004Factory>());

    state.dataIPShortCut->cCurrentModuleObject = CoilCoolingDX205Performance::object_name;
    auto &ip = state.dataInputProcessing->inputProcessor;
    int numPerformances = ip->getNumObjectsFound(state, CoilCoolingDX205Performance::object_name);
    if (numPerformances <= 0) {
        ShowSevereError(state, format("No {} equipment specified in input file", state.dataIPShortCut->cCurrentModuleObject));
        ErrorsFound = true;
    }
    auto const &Coil205PerformanceInstances = ip->epJSON.find(state.dataIPShortCut->cCurrentModuleObject).value();
    auto const &objectSchemaProps = ip->getObjectSchemaProps(state, state.dataIPShortCut->cCurrentModuleObject);

    for (auto &instance : Coil205PerformanceInstances.items()) {
        auto const &fields = instance.value();
        std::string const &thisObjectName = instance.key();

        if (!UtilityRoutines::SameString(name_to_find, thisObjectName)) {
            continue;
        } else {
            ShowFatalError(state, "Could not find Coil:Cooling:DX:Performance object with name: " + name_to_find);
        }

        bool errorsFound(false);

        this->name = ip->getAlphaFieldValue(fields, objectSchemaProps, "name");
        std::string const rep_file_name = ip->getAlphaFieldValue(fields, objectSchemaProps, "representation_file_name");
        fs::path rep_file_path = DataSystemVariables::CheckForActualFilePath(state, fs::path(rep_file_name), std::string(RoutineName));
        if (rep_file_path.empty()) {
            errorsFound = true;
            // Given that several of the following expressions require the representation file to be present, we'll just throw a fatal here.
            // The errorsFound flag is still set to true here so that in the future, if we defer the fatal until later in this routine, it will still
            // be set The CheckForActualFilePath function emits some nice information to the ERR file, so we just need a simple fatal here
            ShowFatalError(state, "Program terminates due to the missing ASHRAE 205 RS0004 representation file.");
        }
        this->representation =
            std::dynamic_pointer_cast<rs0004_ns::RS0004>(RSInstanceFactory::create("RS0004", rep_file_path.string().c_str()));
        if (nullptr == this->representation) {
            ShowSevereError(state, format("{} is not an instance of an ASHRAE205 Chiller.", rep_file_path.string()));
            errorsFound = true;
        }
        this->interpolation_type =
            InterpMethods[UtilityRoutines::MakeUPPERCase(ip->getAlphaFieldValue(fields, objectSchemaProps, "performance_interpolation_method"))];
        this->rated_total_cooling_capacity = fields.at("rated_total_cooling_capacity").get<Real64>();
        this->rated_steady_state_heating_capacity = fields.at("rated_steady_state_heating_capacity").get<Real64>();

        if (errorsFound) {
            ShowFatalError(state,
                           std::string{routineName} + "Errors found in getting " + this->object_name +
                               " input. Preceding condition(s) causes termination.");
        }
    }
}
