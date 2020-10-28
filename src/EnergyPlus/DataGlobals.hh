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

#ifndef DataGlobals_hh_INCLUDED
#define DataGlobals_hh_INCLUDED

// C++ Headers
#include <iosfwd>
#include <string>
#include <functional>

// EnergyPlus Headers
#include "IOFiles.hh"
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace DataGlobals {

    // (last time step of last hour of last day of environ which is a design day)
    extern bool EndDayFlag;                          // True at the end of each day (last time step of last hour of day)
    extern bool EndHourFlag;                         // True at the end of each hour (last time step of hour)
    extern int PreviousHour;                         // Previous Hour Index
    extern int HourOfDay;                            // Counter for hours in a simulation day
    extern Real64 WeightPreviousHour;                // Weighting of value for previous hour
    extern Real64 WeightNow;                         // Weighting of value for current hour
    extern int NumOfDayInEnvrn;                      // Number of days in the simulation for a particular environment
    extern int NumOfTimeStepInHour;                  // Number of time steps in each hour of the simulation
    extern int NumOfZones;                           // Total number of Zones for simulation
    extern int TimeStep;                             // Counter for time steps (fractional hours)
    extern Real64 TimeStepZone;                      // Zone time step in fractional hours
    extern bool WarmupFlag;                          // True during the warmup portion of a simulation
    extern int OutputStandardError;                  // Unit number for the standard error output file
    extern int StdOutputRecordCount;                 // Count of Standard output records
    extern int StdMeterRecordCount;                  // Count of Meter output records
    extern bool ZoneSizingCalc;                      // TRUE if zone sizing calculation
    extern bool SysSizingCalc;                       // TRUE if system sizing calculation
    extern bool DoZoneSizing;                        // User input in SimulationControl object
    extern bool DoSystemSizing;                      // User input in SimulationControl object
    extern bool DoPlantSizing;                       // User input in SimulationControl object
    extern bool DoDesDaySim;                         // User input in SimulationControl object
    extern bool DoWeathSim;                          // User input in SimulationControl object
    extern bool DoHVACSizingSimulation;              // User input in SimulationControl object
    extern int HVACSizingSimMaxIterations;           // User input in SimulationControl object
    extern bool WeathSimReq;                         // Input has a RunPeriod request
    extern bool DoOutputReporting;                   // TRUE if variables to be written out
    extern bool DoingSizing;                         // TRUE when "sizing" is being performed (some error messages won't be displayed)
    extern bool DoingHVACSizingSimulations;          // true when HVAC Sizing Simulations are being performed.
    extern bool DoingInputProcessing;                // TRUE when "IP" is being performed (some error messages are cached)
    extern bool DisplayAllWarnings;                  // True when selection for  "DisplayAllWarnings" is entered (turns on other warning flags)
    extern bool DisplayExtraWarnings;                // True when selection for  "DisplayExtraWarnings" is entered
    extern bool DisplayUnusedObjects;                // True when selection for  "DisplayUnusedObjects" is entered
    extern bool DisplayUnusedSchedules;              // True when selection for  "DisplayUnusedSchedules" is entered
    extern bool DisplayAdvancedReportVariables;      // True when selection for  "DisplayAdvancedReportVariables" is entered
    extern bool DisplayZoneAirHeatBalanceOffBalance; // True when selection for  "DisplayZoneAirHeatBalanceOffBalance" is entered
    extern bool DisplayInputInAudit;                 // True when environmental variable "DisplayInputInAudit" is used
    extern bool CreateMinimalSurfaceVariables;       // True when selection for  "CreateMinimalSurfaceVariables" is entered
    extern Real64 CurrentTime;                       // CurrentTime, in fractional hours, from start of day. Uses Loads time step.
    extern int SimTimeSteps;                         // Number of (Loads) timesteps since beginning of run period (environment).
    extern int MinutesPerTimeStep;                   // Minutes per time step calculated from NumTimeStepInHour (number of minutes per load time step)
    extern Real64 TimeStepZoneSec;                   // Seconds per time step
    extern bool MetersHaveBeenInitialized;
    extern bool KickOffSimulation;                 // Kick off simulation -- meaning run each environment for 1 or 2 time steps.
    extern bool KickOffSizing;                     // Kick off sizing -- meaning run each environment for 1 or 2 time steps.
    extern bool RedoSizesHVACSimulation;           // doing kick off simulation for redoing sizes as part of sizing
    extern bool FinalSizingHVACSizingSimIteration; // true if this will be no more HVAC sizing sim iterations
    extern bool AnyEnergyManagementSystemInModel;  // true if there is any EMS or Erl in model.  otherwise false
    extern bool AnyLocalEnvironmentsInModel;       // true if there is any local environmental data objected defined in model, otherwise false
    extern bool AnyPlantInModel;                   // true if there are any plant or condenser loops in model, otherwise false
    extern bool AnyIdealCondEntSetPointInModel;    // true if there is any ideal condenser entering set point manager in model.
    extern bool RunOptCondEntTemp;                 // true if the ideal condenser entering set point optimization is running
    extern bool CompLoadReportIsReq;               // true if the extra sizing calcs are performed to create a "pulse" for the load component report
    extern bool isPulseZoneSizing;                 // true during the set of zone sizing calcs that include the "pulse" for the load component report
    extern int
        OutputFileZonePulse; // file handle for special zone sizing report that contains the result of the "pulse" for the load component report
    extern bool doLoadComponentPulseNow; // true for the time step that is the "pulse" for the load component report
    extern bool ShowDecayCurvesInEIO;    // true if the Radiant to Convective Decay Curves should appear in the EIO file
    extern bool AnySlabsInModel;         // true if there are any zone-coupled ground domains in the input file
    extern bool AnyBasementsInModel;     // true if there are any basements in the input file
    extern bool DoCoilDirectSolutions;        //true if use coil direction solutions
    extern bool createPerfLog; //true if the _perflog.csv file should be created and a PerformancePrecisionTradeoffs object is used
    extern int Progress;
    extern void (*fProgressPtr)(int const);
    extern void (*fMessagePtr)(std::string const &);
    // these are the new ones
    extern std::function<void(int const)> progressCallback;
    extern std::function<void(const std::string &)> messageCallback;
    extern std::function<void(EnergyPlus::Error, const std::string &)> errorCallback;
    extern bool eplusRunningViaAPI; // a flag for capturing whether we are running via API - if so we can't do python plugins
    // Clears the global data in DataGlobals.
    // Needed for unit tests, should not be normally called.
    void clear_state(EnergyPlus::IOFiles &ioFiles);

} // namespace DataGlobals

    struct DataGlobal : BaseGlobalStruct {
        bool BeginDayFlag = false;                  // True at the start of each day, False after first time step in day
        bool BeginEnvrnFlag = false;                // True at the start of each environment, False after first time step in environ
        bool beginEnvrnWarmStartFlag = false;       // Sizing Speed Up
        bool BeginHourFlag = false;                 // True at the start of each hour, False after first time step in hour
        bool BeginSimFlag = false;                  // True until any actual simulation (full or sizing) has begun, False after first time step
        bool BeginFullSimFlag = false;              // True until full simulation has begun, False after first time step
        bool BeginTimeStepFlag = false;             // True at the start of each time step, False after first subtime step of time step
        int DayOfSim = 0;                           // Counter for days (during the simulation)
        int CalendarYear = 0;                       // Calendar year of the current day of simulation
        std::string CalendarYearChr;                // Calendar year of the current day of simulation (character -- for reporting)
        bool EndEnvrnFlag = false;                  // True at the end of each environment (last time step of last hour of last day of environ)
        bool EndDesignDayEnvrnsFlag = false;        // True at the end of the last design day environment

        bool AnnualSimulation = false;
        std::string DayOfSimChr = "0";              // Counter for days (during the simulation) (character -- for reporting)
        bool runReadVars= false;
        bool DDOnlySimulation= false;
        bool outputEpJSONConversion = false;
        bool outputEpJSONConversionOnly = false;
        bool isEpJSON= false;
        bool isCBOR= false;
        bool isMsgPack= false;
        bool isUBJSON= false;
        bool isBSON= false;
        bool preserveIDFOrder = true;
        bool stopSimulation= false;
        std::function<void (void *)> externalHVACManager;
        bool externalHVACManagerInitialized = false;
        DataGlobalConstants::KindOfSim KindOfSim = DataGlobalConstants::KindOfSim::Unassigned;

        void clear_state() override {
            this->BeginDayFlag = false;
            this->BeginEnvrnFlag = false;
            this->beginEnvrnWarmStartFlag = false;
            this->BeginHourFlag = false;
            this->BeginSimFlag = false;
            this->BeginFullSimFlag = false;
            this->BeginTimeStepFlag = false;
            this->DayOfSim = 0;
            this->CalendarYear = 0;
            this->CalendarYearChr = "0";
            this->EndEnvrnFlag = false;
            this->EndDesignDayEnvrnsFlag = false;

            this->AnnualSimulation = false;
            this->DayOfSimChr = "0";
            this->runReadVars = false;
            this->DDOnlySimulation = false;
            this->outputEpJSONConversion = false;
            this->outputEpJSONConversionOnly = false;
            this->isEpJSON = false;
            this->isCBOR = false;
            this->isMsgPack = false;
            this->isUBJSON = false;
            this->isBSON = false;
            this->preserveIDFOrder = true;
            this->stopSimulation= false;
            this->externalHVACManager = nullptr;
            this->externalHVACManagerInitialized = false;
            KindOfSim = DataGlobalConstants::KindOfSim::Unassigned;
        }
    };

} // namespace EnergyPlus

#endif
