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

//C++ Headers
#include <utility>

// ObjexxFCL Headers
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataSystemVariables {

    // MODULE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   May 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This data-only module is a repository for system (such as environment) variables that are set
    // before a run or set of runs.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // OTHER NOTES:
    // na

    // Using/Aliasing
    using DataStringGlobals::altpathChar;
    using DataStringGlobals::CurrentWorkingFolder;
    using DataStringGlobals::pathChar;
    using DataStringGlobals::ProgramPath;
    using namespace FileSystem;

    // Data
    // -only module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:
    int const iASCII_CR(13);   // endline value when just CR instead of CR/LF
    int const iUnicode_end(0); // endline value when Unicode file
    char const tabchar('\t');

    std::string const DDOnlyEnvVar("DDONLY");       // Only run design days
    std::string const ReverseDDEnvVar("REVERSEDD"); // Reverse DD during run
    std::string const DisableGLHECachingEnvVar("DISABLEGLHECACHING");
    std::string const FullAnnualSimulation("FULLANNUALRUN"); // Generate annual run
    std::string const cDeveloperFlag("DeveloperFlag");
    std::string const cDisplayAllWarnings("DisplayAllWarnings");
    std::string const cDisplayExtraWarnings("DisplayExtraWarnings");
    std::string const cDisplayAdvancedReportVariables("DisplayAdvancedReportVariables");
    std::string const cDisplayUnusedObjects("DisplayUnusedObjects");
    std::string const cDisplayUnusedSchedules("DisplayUnusedSchedules");
    std::string const cDisplayZoneAirHeatBalanceOffBalance("DisplayZoneAirHeatBalanceOffBalance");
    std::string const cSortIDD("SortIDD");
    std::string const cReportDuringWarmup("ReportDuringWarmup");
    std::string const cReportDuringHVACSizingSimulation("REPORTDURINGHVACSIZINGSIMULATION");
    std::string const cIgnoreSolarRadiation("IgnoreSolarRadiation");
    std::string const cIgnoreBeamRadiation("IgnoreBeamRadiation");
    std::string const cIgnoreDiffuseRadiation("IgnoreDiffuseRadiation");
    std::string const cSutherlandHodgman("SutherlandHodgman");
    std::string const cSlaterBarsky("SlaterBarsky");
    std::string const cMinimalSurfaceVariables("CreateMinimalSurfaceVariables");
    std::string const cMinimalShadowing("MinimalShadowing");
    std::string const cNumActiveSims("cntActv");
    std::string const cInputPath1("epin");       // EP-Launch setting.  Full path + project name
    std::string const cInputPath2("input_path"); // RunEplus.bat setting.  Full path
    std::string const cProgramPath("program_path");
    std::string const cTimingFlag("TimingFlag");
    std::string const TrackAirLoopEnvVar("TRACK_AIRLOOP"); // To generate a file with runtime statistics
    // for each controller on each air loop
    std::string const TraceAirLoopEnvVar("TRACE_AIRLOOP"); // To generate a trace file with the converged
    // solutions of all controllers on each air loop at each call to SimAirLoop()
    std::string const TraceHVACControllerEnvVar("TRACE_HVACCONTROLLER"); // To generate a trace file for
    //  each individual HVAC controller with all controller iterations

    std::string const MinReportFrequencyEnvVar("MINREPORTFREQUENCY"); // environment var for reporting frequency.
    std::string const
        cDisplayInputInAuditEnvVar("DISPLAYINPUTINAUDIT"); // environmental variable that enables the echoing of the input file into the audit file

    // DERIVED TYPE DEFINITIONS
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // MODULE VARIABLE DECLARATIONS:
    bool DDOnly(false);                           // TRUE if design days (sizingperiod:*) only are to be run.
    bool ReverseDD(false);                        // TRUE if reverse design days (reordering sizingperiod:*) are to be run.
    bool DisableGLHECaching(false);               // TRUE if caching is to be disabled, for example, during unit tests.
    bool FullAnnualRun(false);                    // TRUE if full annual simulation is to be run.
    bool DeveloperFlag(false);                    // TRUE if developer flag is turned on. (turns on more displays to console)
    bool TimingFlag(false);                       // TRUE if timing flag is turned on. (turns on more timing displays to console)

    // Shading methods
    ShadingMethod shadingMethod(ShadingMethod::PolygonClipping);
    bool SutherlandHodgman(true);                 // TRUE if SutherlandHodgman algorithm for polygon clipping is to be used.
    bool SlaterBarsky(false);                  // TRUE if SlaterBarsky algorithm for polygon clipping is to be used for vertical polygons.
    bool DetailedSkyDiffuseAlgorithm(false);      // use detailed diffuse shading algorithm for sky (shading transmittance varies)
    bool DetailedSolarTimestepIntegration(false); // when true, use detailed timestep integration for all solar,shading, etc.
    bool DisableGroupSelfShading(false); // when true, defined shadowing surfaces group is ignored when calculating sunlit fraction
    bool DisableAllSelfShading(false);   // when true, all external shadowing surfaces is ignored when calculating sunlit fraction


    bool TrackAirLoopEnvFlag(false);              // If TRUE generates a file with runtime statistics for each HVAC
    //  controller on each air loop
    bool TraceAirLoopEnvFlag(false); // If TRUE generates a trace file with the converged solutions of all
    // HVAC controllers on each air loop at each call to SimAirLoop()
    bool TraceHVACControllerEnvFlag(false); // If TRUE generates a trace file for each individual HVAC
    // controller with all controller iterations
    bool ReportDuringWarmup(false);                      // True when the report outputs even during warmup
    bool ReportDuringHVACSizingSimulation(false);        // true when reporting outputs during HVAC sizing Simulation
    bool ReportDetailedWarmupConvergence(false);         // True when the detailed warmup convergence is requested
    bool UpdateDataDuringWarmupExternalInterface(false); // variable sets in the external interface.
    bool ReportExtShadingSunlitFrac(false);              // when true, the sunlit fraction for all surfaces are exported as a csv format output

    // This update the value during the warmup added for FMI
    Real64 Elapsed_Time(0.0);       // For showing elapsed time at end of run
    Real64 Time_Start(0.0);         // Call to CPU_Time for start time of simulation
    Real64 Time_Finish(0.0);        // Call to CPU_Time for end time of simulation
    std::string MinReportFrequency; // String for minimum reporting frequency
    bool SortedIDD(true);           // after processing, use sorted IDD to obtain Defs, etc.
    bool lMinimalShadowing(false);  // TRUE if MinimalShadowing is to override Solar Distribution flag
    std::string envinputpath1;
    std::string envinputpath2;
    bool TestAllPaths(false);
    int iEnvSetThreads(0);
    bool lEnvSetThreadsInput(false);
    int iepEnvSetThreads(0);
    bool lepSetThreadsInput(false);
    int iIDFSetThreads(0);
    bool lIDFSetThreadsInput(false);
    int inumActiveSims(1);
    bool lnumActiveSims(false);
    int MaxNumberOfThreads(1);
    int NumberIntRadThreads(1);
    int iNominalTotSurfaces(0);
    bool Threading(false);
    bool firstTime(true);

    // Functions

    void CheckForActualFileName(EnergyPlusData &state,
                                std::string const &originalInputFileName, // name as input for object
                                bool &FileFound,                          // Set to true if file found and is in CheckedFileName
                                std::string &CheckedFileName,             // Blank if not found.
                                const std::string contextString           //
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // With the Windows version, there are subfolders set and the input file names may not
        // be accurate. This searches a few folders (CurrentWorkingFolder, Program folder) to see
        // if the file can be found. (It may have been input with full path so that is checked first.)

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string InputFileName; // save for changing out path characters
        std::string::size_type pos;

        if (firstTime) {
            state.files.audit.ensure_open(state, "CheckForActualFileName", state.files.outputControl.audit);
            get_environment_variable(cInputPath1, envinputpath1);
            if (!envinputpath1.empty()) {
                pos = index(envinputpath1, pathChar, true); // look backwards for pathChar
                if (pos != std::string::npos) envinputpath1.erase(pos + 1);
            }
            get_environment_variable(cInputPath2, envinputpath2);
            get_environment_variable(cProgramPath, ProgramPath);
            firstTime = false;
        }

        FileFound = false;
        CheckedFileName.clear();
        InputFileName = originalInputFileName;
        makeNativePath(InputFileName);

        std::vector<std::pair<std::string, std::string>> pathsChecked;

        const std::array<std::pair<std::string, std::string>, 7> pathsToCheck = {{
            {InputFileName, "Current Working Directory"},
            {DataStringGlobals::inputDirPathName + InputFileName, "IDF Directory"},
            {DataStringGlobals::exeDirectory + InputFileName, "EnergyPlus Executable Directory"},
            {envinputpath1 + InputFileName, "\"epin\" Environment Variable"},
            {envinputpath2 + InputFileName, "\"input_path\" Environment Variable"},
            {CurrentWorkingFolder + InputFileName, "INI File Directory"},
            {ProgramPath + InputFileName, "\"program\", \"dir\" from INI File"}}
        };

        std::size_t numPathsToNotTest = (TestAllPaths) ? pathsToCheck.size()-2 : pathsToCheck.size();

        for(std::size_t i = 0; i < numPathsToNotTest; i++) {
            if (FileSystem::fileExists(pathsToCheck[i].first)) {
                FileFound = true;
                CheckedFileName = pathsToCheck[i].first;
                print(state.files.audit, "{}={}\n", "found (" + pathsToCheck[i].second +")", getAbsolutePath(CheckedFileName));
                return;
            } else {
                std::pair <std::string,std::string> currentPath(getParentDirectoryPath(getAbsolutePath(pathsToCheck[i].first)), pathsToCheck[i].second);
                bool found = false;
                for(auto path: pathsChecked){
                    if (path.first == currentPath.first){
                        found = true;
                    }
                }
                if (!found){
                    pathsChecked.push_back(currentPath);
                }
                print(state.files.audit, "{}={}\n", "not found (" + pathsToCheck[i].second +")\"", getAbsolutePath(pathsToCheck[i].first));
            }
        }
        if (!FileFound) {
            ShowSevereError(state, contextString+ "\"" + originalInputFileName + "\" not found.");
            ShowContinueError(state, "  Paths searched:");
            for(auto path: pathsChecked){
                ShowContinueError(state, "    " + path.second +": \"" + path.first +"\"");
            }
        }
    }

    void clear_state()
    {
        DDOnly = false;
        ReverseDD = false;
        DisableGLHECaching = false;
        FullAnnualRun = false;
        DeveloperFlag = false;
        TimingFlag = false;
        shadingMethod = ShadingMethod::PolygonClipping;
        SutherlandHodgman = true;
        SlaterBarsky = false;
        DetailedSkyDiffuseAlgorithm = false;
        DetailedSolarTimestepIntegration = false;
        TrackAirLoopEnvFlag = false;
        TraceAirLoopEnvFlag = false;
        TraceHVACControllerEnvFlag = false;
        ReportDuringWarmup = false;
        ReportDuringHVACSizingSimulation = false;
        ReportDetailedWarmupConvergence = false;
        UpdateDataDuringWarmupExternalInterface = false;
        ReportExtShadingSunlitFrac = false;
        DisableGroupSelfShading = false;
        DisableAllSelfShading = false;
        Elapsed_Time = 0.0;
        Time_Start = 0.0;
        Time_Finish = 0.0;
        SortedIDD = true;
        lMinimalShadowing = false;
        TestAllPaths = false;
        iEnvSetThreads = 0;
        lEnvSetThreadsInput = false;
        iepEnvSetThreads = 0;
        lepSetThreadsInput = false;
        iIDFSetThreads = 0;
        lIDFSetThreadsInput = false;
        inumActiveSims = 1;
        lnumActiveSims = false;
        MaxNumberOfThreads = 1;
        NumberIntRadThreads = 1;
        iNominalTotSurfaces = 0;
        Threading = false;
        firstTime = true;
    }

    void processEnvironmentVariables(EnergyPlusData &state) {

        static std::string cEnvValue;

        get_environment_variable(DDOnlyEnvVar, cEnvValue);
        DDOnly = env_var_on(cEnvValue); // Yes or True
        if (state.dataGlobal->DDOnlySimulation) DDOnly = true;

        get_environment_variable(ReverseDDEnvVar, cEnvValue);
        ReverseDD = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(DisableGLHECachingEnvVar, cEnvValue);
        DisableGLHECaching = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(FullAnnualSimulation, cEnvValue);
        FullAnnualRun = env_var_on(cEnvValue); // Yes or True
        if (state.dataGlobal->AnnualSimulation) FullAnnualRun = true;

        get_environment_variable(cDisplayAllWarnings, cEnvValue);
        state.dataGlobal->DisplayAllWarnings = env_var_on(cEnvValue); // Yes or True
        if (state.dataGlobal->DisplayAllWarnings) {
            state.dataGlobal->DisplayAllWarnings = true;
            state.dataGlobal->DisplayExtraWarnings = true;
            state.dataGlobal->DisplayUnusedSchedules = true;
            state.dataGlobal->DisplayUnusedObjects = true;
        }

        get_environment_variable(cDisplayExtraWarnings, cEnvValue);
        if (!cEnvValue.empty()) state.dataGlobal->DisplayExtraWarnings = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cDisplayUnusedObjects, cEnvValue);
        if (!cEnvValue.empty()) state.dataGlobal->DisplayUnusedObjects = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cDisplayUnusedSchedules, cEnvValue);
        if (!cEnvValue.empty()) state.dataGlobal->DisplayUnusedSchedules = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cDisplayZoneAirHeatBalanceOffBalance, cEnvValue);
        if (!cEnvValue.empty()) state.dataGlobal->DisplayZoneAirHeatBalanceOffBalance = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cDisplayAdvancedReportVariables, cEnvValue);
        if (!cEnvValue.empty()) state.dataGlobal->DisplayAdvancedReportVariables = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cReportDuringWarmup, cEnvValue);
        if (!cEnvValue.empty()) ReportDuringWarmup = env_var_on(cEnvValue); // Yes or True
        if (ReverseDD) ReportDuringWarmup = false;                          // force to false for ReverseDD runs

        get_environment_variable(cReportDuringWarmup, cEnvValue);
        if (!cEnvValue.empty()) ReportDuringWarmup = env_var_on(cEnvValue); // Yes or True
        if (DisableGLHECaching) ReportDuringWarmup = true;                  // force to true for standard runs runs

        get_environment_variable(cReportDuringHVACSizingSimulation, cEnvValue);
        if (!cEnvValue.empty()) ReportDuringHVACSizingSimulation = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cIgnoreSolarRadiation, cEnvValue);
        if (!cEnvValue.empty()) state.dataEnvrn->IgnoreSolarRadiation = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cMinimalSurfaceVariables, cEnvValue);
        if (!cEnvValue.empty()) state.dataGlobal->CreateMinimalSurfaceVariables = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cSortIDD, cEnvValue);
        if (!cEnvValue.empty()) SortedIDD = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(MinReportFrequencyEnvVar, cEnvValue);
        if (!cEnvValue.empty()) MinReportFrequency = cEnvValue; // turned into value later

        get_environment_variable(cDeveloperFlag, cEnvValue);
        if (!cEnvValue.empty()) DeveloperFlag = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cIgnoreBeamRadiation, cEnvValue);
        if (!cEnvValue.empty()) state.dataEnvrn->IgnoreBeamRadiation = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cIgnoreDiffuseRadiation, cEnvValue);
        if (!cEnvValue.empty()) state.dataEnvrn->IgnoreDiffuseRadiation = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cSutherlandHodgman, cEnvValue);
        if (!cEnvValue.empty()) SutherlandHodgman = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cSlaterBarsky, cEnvValue);
        if (!cEnvValue.empty()) SlaterBarsky = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cMinimalShadowing, cEnvValue);
        if (!cEnvValue.empty()) lMinimalShadowing = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cTimingFlag, cEnvValue);
        if (!cEnvValue.empty()) TimingFlag = env_var_on(cEnvValue); // Yes or True

        // Initialize env flags for air loop simulation debugging
        get_environment_variable(TrackAirLoopEnvVar, cEnvValue);
        if (!cEnvValue.empty()) TrackAirLoopEnvFlag = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(TraceAirLoopEnvVar, cEnvValue);
        if (!cEnvValue.empty()) TraceAirLoopEnvFlag = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(TraceHVACControllerEnvVar, cEnvValue);
        if (!cEnvValue.empty()) TraceHVACControllerEnvFlag = env_var_on(cEnvValue); // Yes or True

        get_environment_variable(cDisplayInputInAuditEnvVar, cEnvValue);
        if (!cEnvValue.empty()) state.dataGlobal->DisplayInputInAudit = env_var_on(cEnvValue); // Yes or True

    }

} // namespace DataSystemVariables

} // namespace EnergyPlus
