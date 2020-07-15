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

#ifndef EPLUS_PLUGIN_MANAGER_HH
#define EPLUS_PLUGIN_MANAGER_HH

#include <iomanip>
#include <queue>
#include <utility>
#include <vector>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

typedef void* PyObjectWrap;

namespace EnergyPlus {

namespace PluginManagement {

    void registerNewCallback(int iCalledFrom, const std::function<void ()>& f);
    void runAnyRegisteredCallbacks(int iCalledFrom, bool &anyRan);
    void onBeginEnvironment();
    std::string pythonStringForUsage();

    void clear_state();

    struct PluginInstance {
        PluginInstance(const std::string& _moduleName, const std::string& _className, std::string emsName, bool runPluginDuringWarmup) :
                emsAlias(std::move(emsName)), runDuringWarmup(runPluginDuringWarmup)
        {
            this->moduleName = _moduleName;
            this->className = _className;
            this->stringIdentifier = _moduleName + "." + _className;
        }

        // members
        std::string moduleName;
        std::string className;
        std::string emsAlias;
        bool runDuringWarmup;
        std::string stringIdentifier; // for diagnostic reporting
#if LINK_WITH_PYTHON
        PyObjectWrap pModule = nullptr;  // reference to module
        PyObjectWrap pClassInstance = nullptr; // reference to instantiated class -- *don't decref until the end of the simulation*
#endif

        // setup/shutdown should only be called once construction is completely done, i.e., setup() should only be called once the vector holding all the
        // instances is done for the day, and shutdown should only be called when you are ready to destruct all the instances.  The things that happen
        // inside setup() and shutdown() are related to un-managed memory, and it's tricky to manage inside existing constructor/move operations, so they
        // are split out into these explicitly called methods.
        void setup();
        void shutdown() const;

        // methods
        static void reportPythonError();
        bool run(int iCallingPoint) const; // calls main() on this plugin instance

        // plugin calling point hooks
        const char * sHookBeginNewEnvironment = "on_begin_new_environment";
        bool bHasBeginNewEnvironment = false;
        const char * sHookAfterNewEnvironmentWarmUpIsComplete = "on_after_new_environment_warmup_is_complete";
        bool bHasAfterNewEnvironmentWarmUpIsComplete = false;
        const char * sHookBeginZoneTimestepBeforeInitHeatBalance = "on_begin_zone_timestep_before_init_heat_balance";
        bool bHasBeginZoneTimestepBeforeInitHeatBalance = false;
        const char * sHookBeginZoneTimestepAfterInitHeatBalance = "on_begin_zone_timestep_after_init_heat_balance";
        bool bHasBeginZoneTimestepAfterInitHeatBalance = false;
        const char * sHookBeginTimestepBeforePredictor = "on_begin_timestep_before_predictor";
        bool bHasBeginTimestepBeforePredictor = false;
        const char * sHookAfterPredictorBeforeHVACManagers = "on_after_predictor_before_hvac_managers";
        bool bHasAfterPredictorBeforeHVACManagers = false;
        const char * sHookAfterPredictorAfterHVACManagers = "on_after_predictor_after_hvac_managers";
        bool bHasAfterPredictorAfterHVACManagers = false;
        const char * sHookInsideHVACSystemIterationLoop = "on_inside_hvac_system_iteration_loop";
        bool bHasInsideHVACSystemIterationLoop = false;
        const char * sHookEndOfZoneTimestepBeforeZoneReporting = "on_end_of_zone_timestep_before_zone_reporting";
        bool bHasEndOfZoneTimestepBeforeZoneReporting = false;
        const char * sHookEndOfZoneTimestepAfterZoneReporting = "on_end_of_zone_timestep_after_zone_reporting";
        bool bHasEndOfZoneTimestepAfterZoneReporting = false;
        const char * sHookEndOfSystemTimestepBeforeHVACReporting = "on_end_of_system_timestep_before_hvac_reporting";
        bool bHasEndOfSystemTimestepBeforeHVACReporting = false;
        const char * sHookEndOfSystemTimestepAfterHVACReporting = "on_end_of_system_timestep_after_hvac_reporting";
        bool bHasEndOfSystemTimestepAfterHVACReporting = false;
        const char * sHookEndOfZoneSizing = "on_end_of_zone_sizing";
        bool bHasEndOfZoneSizing = false;
        const char * sHookEndOfSystemSizing = "on_end_of_system_sizing";
        bool bHasEndOfSystemSizing = false;
        const char * sHookAfterComponentInputReadIn = "on_end_of_component_input_read_in";
        bool bHasAfterComponentInputReadIn = false;
        const char * sHookUserDefinedComponentModel = "on_user_defined_component_model";
        bool bHasUserDefinedComponentModel = false;
        const char * sHookUnitarySystemSizing = "on_unitary_system_sizing";
        bool bHasUnitarySystemSizing = false;
    };

    class PluginManager {
    public:
        PluginManager();
        ~PluginManager();

        static int numActiveCallbacks();
        static void addToPythonPath(const std::string& path, bool userDefinedPath);
        static std::string sanitizedPath(std::string path); // intentionally not a const& string
        static void setupOutputVariables();

        int maxGlobalVariableIndex = -1;
        void addGlobalVariable(const std::string& name);
        static int getGlobalVariableHandle(const std::string& name, bool suppress_warning = false);
        static Real64 getGlobalVariableValue(int handle);
        static void setGlobalVariableValue(int handle, Real64 value);

        int maxTrendVariableIndex = -1;
        static int getTrendVariableHandle(const std::string& name);
        static Real64 getTrendVariableValue(int handle, int timeIndex);
        static size_t getTrendVariableHistorySize(int handle);
        static Real64 getTrendVariableAverage(int handle, int count);
        static Real64 getTrendVariableMin(int handle, int count);
        static Real64 getTrendVariableMax(int handle, int count);
        static Real64 getTrendVariableSum(int handle, int count);
        static Real64 getTrendVariableDirection(int handle, int count);

        static void updatePluginValues();

        static int getLocationOfUserDefinedPlugin(std::string const &programName);
        static void runSingleUserDefinedPlugin(int index);
        static bool anyUnexpectedPluginObjects();
    };

    struct PluginTrendVariable {
        std::string name;
        int numValues;
        std::deque<Real64> values;
        std::deque<Real64> times;
        int indexOfPluginVariable;
        PluginTrendVariable(std::string _name, int _numValues, int _indexOfPluginVariable) :
            name(std::move(_name)), numValues(_numValues), indexOfPluginVariable(_indexOfPluginVariable)
        {
            // initialize the deque so it can be queried immediately, even with just zeroes
            for (int i = 1; i <= this->numValues; i++) {
                this->values.push_back(0);
            }
            for (int loop = 1; loop <= _numValues; ++loop) {
                this->times.push_back(-loop * DataGlobals::TimeStepZone);
            }
        }
        void reset() {
            this->values.clear();
            for (int i = 1; i <= this->numValues; i++) {
                this->values.push_back(0);
            }
        }
    };

    extern std::unique_ptr<PluginManager> pluginManager;
    extern std::vector<PluginTrendVariable> trends;
    extern std::vector<std::string> globalVariableNames;
    extern std::vector<Real64> globalVariableValues;

    // some flags
    extern bool fullyReady;
    extern bool apiErrorFlag;

}
}

#endif // EPLUS_PLUGIN_MANAGER_HH
