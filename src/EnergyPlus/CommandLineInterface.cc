// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <thread>

// CLI Headers
#include <ezOptionParser.hpp>

// Project headers
#include <EnergyPlus/CommandLineInterface.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace CommandLineInterface {

    using namespace ez;

    int ProcessArgs(EnergyPlusData &state, int argc, const char *argv[])
    {
        typedef std::string::size_type size_type;

        // Expand long-name options using "=" sign in to two arguments
        // and expand multiple short options into separate arguments
        std::vector<std::string> arguments;

        for (int i = 0; i < argc; ++i) {

            std::string inputArg(argv[i]);

            std::string const dash("-");
            size_type const doubleDashPosition = inputArg.find("--");
            size_type const equalsPosition = inputArg.find("=");

            if (doubleDashPosition == 0 && equalsPosition != std::string::npos) { // --option=value
                arguments.push_back(inputArg.substr(0, equalsPosition));
                arguments.push_back(inputArg.substr(equalsPosition + 1, inputArg.size() - 1));
            } else if ((inputArg.size() > 2) && (inputArg[0] == '-') && (inputArg[1] != '-')) { // -abc style
                for (size_type c = 1; c < inputArg.size(); ++c) {
                    arguments.push_back(dash + inputArg[c]);
                }
            } else { // ?
                arguments.push_back(inputArg);
            }
        }

        // Fix This is problematic for a few reasons:
        //  Using ezOptionParser with a raw C-string interface is asking for trouble: Find something taking std::string if possible
        //  Passing out pointers returned by c_str() is bad form:
        //   They are pointers to internally-managed memory in std::string
        //   They are invalid as soon as the string goes out of scope or is modified
        //   In this case the strings may be in scope and unmodified until parse is done but this is red flag usage
        // convert to vector of C strings for option parser
        std::vector<const char *> cStrArgs;
        cStrArgs.reserve(arguments.size());
        for (size_type i = 0; i < arguments.size(); ++i) {
            cStrArgs.push_back(arguments[i].c_str());
        }

        size_type const argCount = cStrArgs.size();

        bool const legacyMode = (argCount == 1);

        // Define options
        ezOptionParser opt;

        opt.overview = state.dataStrGlobals->VerStringVar;
        opt.overview.append("\nPythonLinkage: " + PluginManagement::pythonStringForUsage(state));
        opt.overview.append("\nBuilt on Platform: " + DataStringGlobals::BuildPlatformString);

        opt.syntax = "energyplus [options] [input-file]";

        opt.add("", 0, 0, 0, "Force annual simulation", "-a", "--annual");

        opt.add("", 0, 1, 0, "Output directory path (default: current directory)", "-d", "--output-directory");

        opt.add("", 0, 0, 0, "Force design-day-only simulation", "-D", "--design-day");

        opt.add("", 0, 0, 0, "Display help information", "-h", "--help");

        opt.add("Energy+.idd", 0, 1, 0, "Input data dictionary path (default: Energy+.idd in executable directory)", "-i", "--idd");

        opt.add("", 0, 0, 0, "Run EPMacro prior to simulation", "-m", "--epmacro");

        opt.add("", 0, 1, 0, "Prefix for output file names (default: eplus)", "-p", "--output-prefix");

        opt.add("", 0, 0, 0, "Run ReadVarsESO after simulation", "-r", "--readvars");

        opt.add("", 0, 0, 0, "Output IDF->epJSON or epJSON->IDF, dependent on input file type", "-c", "--convert");

        opt.add("", 0, 0, 0, "Only convert IDF->epJSON or epJSON->IDF, dependent on input file type. No simulation", "--convert-only");

        opt.add("L",
                0,
                1,
                0,
                "Suffix style for output file names (default: L)\n   L: Legacy (e.g., eplustbl.csv)\n   C: Capital (e.g., eplusTable.csv)\n   D: "
                "Dash (e.g., eplus-table.csv)",
                "-s",
                "--output-suffix");

        opt.add("", false, 0, 0, "Display version information", "-v", "--version");

        opt.add("1", false, 1, 0, "Multi-thread with N threads; 1 thread with no arg. (Currently only for G-Function generation)", "-j", "--jobs");

        opt.add("in.epw", false, 1, 0, "Weather file path (default: in.epw in current directory)", "-w", "--weather");

        opt.add("", false, 0, 0, "Run ExpandObjects prior to simulation", "-x", "--expandobjects");

        opt.example = "energyplus -w weather.epw -r input.idf";

        std::string errorFollowUp = "Type 'energyplus --help' for usage.";

        // Parse arguments
        opt.parse(argCount, &cStrArgs[0]);

        // print arguments parsed (useful for debugging)
        //        std::string pretty;
        //        opt.prettyPrint(pretty);
        //        std::cout << pretty << std::endl;

        std::string usage;
        opt.getUsage(usage);

        // Set path of EnergyPlus program path (if we aren't overriding it)
        if (!state.dataGlobal->installRootOverride) {
            state.dataStrGlobals->exeDirectoryPath = FileSystem::getParentDirectoryPath(FileSystem::getAbsolutePath(FileSystem::getProgramPath()));
        }

        // ezOptionParser doesn't have a getPath, so we get a string, then convert it to path
        // I'm limiting the scope of the std::string temporaries I used to avoid mistakes
        {
            std::string inputWeatherFileName;
            opt.get("-w")->getString(inputWeatherFileName);
            state.files.inputWeatherFilePath.filePath = fs::path{inputWeatherFileName};
        }

        {
            // TODO: should this be in IOFiles as an InputFile?
            std::string inputIddFileName;
            opt.get("-i")->getString(inputIddFileName);
            state.dataStrGlobals->inputIddFilePath = fs::path{inputIddFileName};
        }

        if (!opt.isSet("-i") && !legacyMode) {
            state.dataStrGlobals->inputIddFilePath = state.dataStrGlobals->exeDirectoryPath / state.dataStrGlobals->inputIddFilePath;
        }

        {
            std::string outDirPathName;
            opt.get("-d")->getString(outDirPathName);
            state.dataStrGlobals->outDirPath = fs::path{outDirPathName};
        }

        state.dataGlobal->runReadVars = opt.isSet("-r");

        state.dataGlobal->DDOnlySimulation = opt.isSet("-D");

        state.dataGlobal->AnnualSimulation = opt.isSet("-a");

        state.dataGlobal->outputEpJSONConversion = opt.isSet("-c");

        state.dataGlobal->outputEpJSONConversionOnly = opt.isSet("--convert-only");

        bool eplusRunningViaAPI = state.dataGlobal->eplusRunningViaAPI;

        opt.get("-j")->getInt(state.dataGlobal->numThread);

        if (state.dataGlobal->numThread == 0) {
            DisplayString(state, "Invalid value for -j arg. Defaulting to 1.");
            state.dataGlobal->numThread = 1;
        } else if (state.dataGlobal->numThread > (int)std::thread::hardware_concurrency()) {
            DisplayString(state,
                          fmt::format("Invalid value for -j arg. Value exceeds num available. Defaulting to num available. -j {}",
                                      (int)std::thread::hardware_concurrency()));
            state.dataGlobal->numThread = (int)std::thread::hardware_concurrency();
        }

        // Process standard arguments
        if (opt.isSet("-h")) {
            DisplayString(state, usage);
            if (eplusRunningViaAPI) {
                // we need another return code to let runEnergyPlusAsLibrary know it should not try to run anything
                return static_cast<int>(ReturnCodes::SuccessButHelper);
            } else {
                exit(EXIT_SUCCESS);
            }
        }

        if (opt.isSet("-v")) {
            DisplayString(state, state.dataStrGlobals->VerStringVar);
            if (eplusRunningViaAPI) {
                // we need another return code to let runEnergyPlusAsLibrary know it should not try to run anything
                return static_cast<int>(ReturnCodes::SuccessButHelper);
            } else {
                exit(EXIT_SUCCESS);
            }
        }

        if (opt.lastArgs.size() == 1) {
            state.dataStrGlobals->inputFilePath = fs::path{*opt.lastArgs[0]};
        } else if (opt.lastArgs.empty()) {
            state.dataStrGlobals->inputFilePath = "in.idf";
        }

        // Convert all paths to native paths
        state.dataStrGlobals->inputFilePath = FileSystem::makeNativePath(state.dataStrGlobals->inputFilePath);
        state.files.inputWeatherFilePath.filePath = FileSystem::makeNativePath(state.files.inputWeatherFilePath.filePath);
        state.dataStrGlobals->inputIddFilePath = FileSystem::makeNativePath(state.dataStrGlobals->inputIddFilePath);
        state.dataStrGlobals->outDirPath = FileSystem::makeNativePath(state.dataStrGlobals->outDirPath);

        std::vector<std::string> badOptions;
        if (opt.lastArgs.size() > 1u) {
            bool invalidOptionFound = false;
            for (size_type i = 0; i < opt.lastArgs.size(); ++i) {
                std::string const &arg(*opt.lastArgs[i]);
                if (arg.substr(0, 1) == "-") {
                    invalidOptionFound = true;
                    DisplayString(state, "ERROR: Invalid option: " + arg);
                }
            }
            if (invalidOptionFound) {
                DisplayString(state, errorFollowUp);
                if (eplusRunningViaAPI) {
                    return static_cast<int>(ReturnCodes::Failure);
                } else {
                    exit(EXIT_FAILURE);
                }
            } else {
                DisplayString(state, "ERROR: Multiple input files specified:");
                for (size_type i = 0; i < opt.lastArgs.size(); ++i) {
                    std::string const &arg(*opt.lastArgs[i]);
                    DisplayString(state, format("  Input file #{}: {}", i + 1, arg));
                }
                DisplayString(state, errorFollowUp);
                if (eplusRunningViaAPI) {
                    return static_cast<int>(ReturnCodes::Failure);
                } else {
                    exit(EXIT_FAILURE);
                }
            }
        }

        state.dataStrGlobals->inputFilePathNameOnly = FileSystem::getFileName(state.dataStrGlobals->inputFilePath);
        state.dataStrGlobals->inputDirPath = FileSystem::getParentDirectoryPath(state.dataStrGlobals->inputFilePath);

        {
            auto const fileType = FileSystem::getFileType(state.dataStrGlobals->inputFilePath);
            state.dataGlobal->isEpJSON = FileSystem::is_all_json_type(fileType);
            switch (fileType) {
            case FileSystem::FileTypes::IDF:
            case FileSystem::FileTypes::IMF:
            case FileSystem::FileTypes::EpJSON:
            case FileSystem::FileTypes::JSON:
                break;
            case FileSystem::FileTypes::CBOR:
                state.dataGlobal->isCBOR = true;
                DisplayString(state, "CBOR input format is experimental and unsupported.");
                break;
            case FileSystem::FileTypes::MsgPack:
                state.dataGlobal->isMsgPack = true;
                DisplayString(state, "MsgPack input format is experimental and unsupported.");
                break;
            case FileSystem::FileTypes::UBJSON:
                state.dataGlobal->isUBJSON = true;
                DisplayString(state, "UBJSON input format is experimental and unsupported.");
                break;
            case FileSystem::FileTypes::BSON:
                state.dataGlobal->isBSON = true;
                DisplayString(state, "BSON input format is experimental and unsupported.");
                break;
            default:
                DisplayString(state, "ERROR: Input file must have IDF, IMF, or epJSON extension.");
                if (eplusRunningViaAPI) {
                    return static_cast<int>(ReturnCodes::Failure);
                } else {
                    exit(EXIT_FAILURE);
                }
            }
        }

        bool runExpandObjects = opt.isSet("-x");
        bool runEPMacro = opt.isSet("-m");

        if (opt.isSet("-d")) {
            // Create directory if it doesn't already exist
            FileSystem::makeDirectory(state.dataStrGlobals->outDirPath);
        }

        // File naming scheme
        fs::path outputFilePrefixFullPath;
        if (opt.isSet("-p")) {
            std::string prefixOutName;
            opt.get("-p")->getString(prefixOutName);
            fs::path prefix = FileSystem::makeNativePath(fs::path(prefixOutName)); // Why is this needed?
            outputFilePrefixFullPath = state.dataStrGlobals->outDirPath / prefixOutName;
        } else {
            outputFilePrefixFullPath = state.dataStrGlobals->outDirPath / "eplus";
        }

        fs::path outputEpmdetFilePath;
        fs::path outputEpmidfFilePath;

        fs::path outputExpidfFilePath;
        fs::path outputExperrFilePath;

        std::string normalSuffix;
        std::string tableSuffix;
        std::string mapSuffix;
        std::string zszSuffix;
        std::string sszSuffix;
        std::string meterSuffix;
        std::string sqliteSuffix;
        std::string adsSuffix;
        std::string screenSuffix;
        std::string shdSuffix;

        std::string suffixType;
        {
            opt.get("-s")->getString(suffixType);
            std::transform((suffixType).begin(), (suffixType).end(), (suffixType).begin(), ::toupper);

            if (suffixType == "L") {

                normalSuffix = "out";
                tableSuffix = "tbl";
                mapSuffix = "map";
                zszSuffix = "zsz";
                sszSuffix = "ssz";
                meterSuffix = "mtr";
                sqliteSuffix = "sqlite";
                adsSuffix = "ADS";
                screenSuffix = "screen";
                shdSuffix = "shading";

            } else if (suffixType == "D") {

                normalSuffix = "";
                tableSuffix = "-table";
                mapSuffix = "-map";
                zszSuffix = "-zsz";
                sszSuffix = "-ssz";
                meterSuffix = "-meter";
                sqliteSuffix = "-sqlite";
                adsSuffix = "-ads";
                screenSuffix = "-screen";
                shdSuffix = "-shading";

            } else if (suffixType == "C") {

                normalSuffix = "";
                tableSuffix = "Table";
                mapSuffix = "Map";
                zszSuffix = "Zsz";
                sszSuffix = "Ssz";
                meterSuffix = "Meter";
                sqliteSuffix = "Sqlite";
                adsSuffix = "Ads";
                screenSuffix = "Screen";
                shdSuffix = "Shading";

            } else {
                DisplayString(state, "ERROR: Unrecognized argument for output suffix style: " + suffixType);
                DisplayString(state, errorFollowUp);
                if (eplusRunningViaAPI) {
                    return static_cast<int>(ReturnCodes::Failure);
                } else {
                    exit(EXIT_FAILURE);
                }
            }
        }

        // Helper to construct output file path
        auto composePath = [&outputFilePrefixFullPath](const std::string &suffix) -> fs::path {
            return fs::path(outputFilePrefixFullPath.string() + suffix);
        };

        // EnergyPlus files
        state.files.audit.filePath = composePath(normalSuffix + ".audit");
        state.files.bnd.filePath = composePath(normalSuffix + ".bnd");
        state.files.dxf.filePath = composePath(normalSuffix + ".dxf");
        state.files.eio.filePath = composePath(normalSuffix + ".eio");
        state.files.endFile.filePath = composePath(normalSuffix + ".end");
        state.files.outputErrFilePath = composePath(normalSuffix + ".err");
        state.files.eso.filePath = composePath(normalSuffix + ".eso");

        state.files.json.outputJsonFilePath = composePath(normalSuffix + ".json");
        state.files.json.outputTSZoneJsonFilePath = composePath(normalSuffix + "_detailed_zone.json");
        state.files.json.outputTSHvacJsonFilePath = composePath(normalSuffix + "_detailed_HVAC.json");
        state.files.json.outputTSJsonFilePath = composePath(normalSuffix + "_timestep.json");
        state.files.json.outputYRJsonFilePath = composePath(normalSuffix + "_yearly.json");
        state.files.json.outputMNJsonFilePath = composePath(normalSuffix + "_monthly.json");
        state.files.json.outputDYJsonFilePath = composePath(normalSuffix + "_daily.json");
        state.files.json.outputHRJsonFilePath = composePath(normalSuffix + "_hourly.json");
        state.files.json.outputSMJsonFilePath = composePath(normalSuffix + "_runperiod.json");
        state.files.json.outputCborFilePath = composePath(normalSuffix + ".cbor");
        state.files.json.outputTSZoneCborFilePath = composePath(normalSuffix + "_detailed_zone.cbor");
        state.files.json.outputTSHvacCborFilePath = composePath(normalSuffix + "_detailed_HVAC.cbor");
        state.files.json.outputTSCborFilePath = composePath(normalSuffix + "_timestep.cbor");
        state.files.json.outputYRCborFilePath = composePath(normalSuffix + "_yearly.cbor");
        state.files.json.outputMNCborFilePath = composePath(normalSuffix + "_monthly.cbor");
        state.files.json.outputDYCborFilePath = composePath(normalSuffix + "_daily.cbor");
        state.files.json.outputHRCborFilePath = composePath(normalSuffix + "_hourly.cbor");
        state.files.json.outputSMCborFilePath = composePath(normalSuffix + "_runperiod.cbor");
        state.files.json.outputMsgPackFilePath = composePath(normalSuffix + ".msgpack");
        state.files.json.outputTSZoneMsgPackFilePath = composePath(normalSuffix + "_detailed_zone.msgpack");
        state.files.json.outputTSHvacMsgPackFilePath = composePath(normalSuffix + "_detailed_HVAC.msgpack");
        state.files.json.outputTSMsgPackFilePath = composePath(normalSuffix + "_timestep.msgpack");
        state.files.json.outputYRMsgPackFilePath = composePath(normalSuffix + "_yearly.msgpack");
        state.files.json.outputMNMsgPackFilePath = composePath(normalSuffix + "_monthly.msgpack");
        state.files.json.outputDYMsgPackFilePath = composePath(normalSuffix + "_daily.msgpack");
        state.files.json.outputHRMsgPackFilePath = composePath(normalSuffix + "_hourly.msgpack");
        state.files.json.outputSMMsgPackFilePath = composePath(normalSuffix + "_runperiod.msgpack");

        state.files.mtd.filePath = composePath(normalSuffix + ".mtd");
        state.files.mdd.filePath = composePath(normalSuffix + ".mdd");
        state.files.mtr.filePath = composePath(normalSuffix + ".mtr");
        state.files.rdd.filePath = composePath(normalSuffix + ".rdd");
        state.dataStrGlobals->outputShdFilePath = composePath(normalSuffix + ".shd");
        state.files.dfs.filePath = composePath(normalSuffix + ".dfs");
        state.dataStrGlobals->outputGLHEFilePath = composePath(normalSuffix + ".glhe");
        state.files.edd.filePath = composePath(normalSuffix + ".edd");
        state.dataStrGlobals->outputIperrFilePath = composePath(normalSuffix + ".iperr");
        state.files.sln.filePath = composePath(normalSuffix + ".sln");
        state.files.sci.filePath = composePath(normalSuffix + ".sci");
        state.files.wrl.filePath = composePath(normalSuffix + ".wrl");
        state.dataStrGlobals->outputSqlFilePath = composePath(normalSuffix + ".sql");
        state.files.debug.filePath = composePath(normalSuffix + ".dbg");
        state.dataStrGlobals->outputPerfLogFilePath = composePath(normalSuffix + "_perflog.csv");
        state.dataStrGlobals->outputTblCsvFilePath = composePath(tableSuffix + ".csv");
        state.dataStrGlobals->outputTblHtmFilePath = composePath(tableSuffix + ".htm");
        state.dataStrGlobals->outputTblTabFilePath = composePath(tableSuffix + ".tab");
        state.dataStrGlobals->outputTblTxtFilePath = composePath(tableSuffix + ".txt");
        state.dataStrGlobals->outputTblXmlFilePath = composePath(tableSuffix + ".xml");
        state.files.outputMapTabFilePath = composePath(mapSuffix + ".tab");
        state.files.outputMapCsvFilePath = composePath(mapSuffix + ".csv");
        state.files.outputMapTxtFilePath = composePath(mapSuffix + ".txt");
        state.files.outputZszCsvFilePath = composePath(zszSuffix + ".csv");
        state.files.outputZszTabFilePath = composePath(zszSuffix + ".tab");
        state.files.outputZszTxtFilePath = composePath(zszSuffix + ".txt");
        state.files.outputSszCsvFilePath = composePath(sszSuffix + ".csv");
        state.files.outputSszTabFilePath = composePath(sszSuffix + ".tab");
        state.files.outputSszTxtFilePath = composePath(sszSuffix + ".txt");
        state.dataStrGlobals->outputAdsFilePath = composePath(adsSuffix + ".out");
        state.files.shade.filePath = composePath(shdSuffix + ".csv");
        if (suffixType == "L") {
            // out/sqlite.err
            state.dataStrGlobals->outputSqliteErrFilePath = state.dataStrGlobals->outDirPath / fs::path{sqliteSuffix + ".err"};
        } else {
            // if 'D':  out/eplus-sqlite.err
            state.dataStrGlobals->outputSqliteErrFilePath = composePath(sqliteSuffix + ".err");
        }

        state.files.screenCsv.filePath = composePath(screenSuffix + ".csv");
        // TODO, why are these relative paths?
        state.files.delightIn.filePath = "eplusout.delightin";
        state.dataStrGlobals->outputDelightOutFilePath = "eplusout.delightout";

        // TODO: why is this relative?
        state.files.iniFile.filePath = "Energy+.ini";

        // Stat next to epw
        // Careful! fs::path::replace_extension will **mutate the original object**
        state.files.inStatFilePath.filePath = state.files.inputWeatherFilePath.filePath;
        state.files.inStatFilePath.filePath.replace_extension(".stat");
        // Or is it better to provide a helper that does not mutate like this?
        // state.files.inStatFilePath.filePath = FileSystem::replaceFileExtension(state.inputWeatherFilePath.filePath, ".stat");

        state.dataStrGlobals->eplusADSFilePath = state.dataStrGlobals->inputDirPath / "eplusADS.inp";

        // Readvars files
        state.files.csv.filePath = composePath(normalSuffix + ".csv");
        state.files.mtr_csv.filePath = composePath(meterSuffix + ".csv");
        state.dataStrGlobals->outputRvauditFilePath = composePath(normalSuffix + ".rvaudit");

        // EPMacro files
        outputEpmdetFilePath = composePath(normalSuffix + ".epmdet");
        outputEpmidfFilePath = composePath(normalSuffix + ".epmidf");

        // ExpandObjects files
        outputExpidfFilePath = composePath(normalSuffix + ".expidf");
        outputExperrFilePath = composePath(normalSuffix + ".experr");

        // Handle bad options
        if (!opt.gotExpected(badOptions)) {
            for (size_type i = 0; i < badOptions.size(); ++i) {
                DisplayString(state, "ERROR: Unexpected number of arguments for option " + badOptions[i]);
            }
            DisplayString(state, errorFollowUp);
            if (eplusRunningViaAPI) {
                return static_cast<int>(ReturnCodes::Failure);
            } else {
                exit(EXIT_FAILURE);
            }
        }

        // This is a placeholder in case there are required options in the future
        if (!opt.gotRequired(badOptions)) {
            for (size_type i = 0; i < badOptions.size(); ++i) {
                DisplayString(state, "ERROR: Missing required option " + badOptions[i]);
            }
            DisplayString(state, errorFollowUp);
            if (eplusRunningViaAPI) {
                return static_cast<int>(ReturnCodes::Failure);
            } else {
                exit(EXIT_FAILURE);
            }
        }

        if (opt.firstArgs.size() > 1 || opt.unknownArgs.size() > 0) {
            for (size_type i = 1; i < opt.firstArgs.size(); ++i) {
                std::string const &arg(*opt.firstArgs[i]);
                DisplayString(state, "ERROR: Invalid option: " + arg);
            }
            for (size_type i = 0; i < opt.unknownArgs.size(); ++i) {
                std::string const &arg(*opt.unknownArgs[i]);
                DisplayString(state, "ERROR: Invalid option: " + arg);
            }
            DisplayString(state, errorFollowUp);
            if (eplusRunningViaAPI) {
                return static_cast<int>(ReturnCodes::Failure);
            } else {
                exit(EXIT_FAILURE);
            }
        }

        // Error for cases where both design-day and annual simulation switches are set
        if (state.dataGlobal->DDOnlySimulation && state.dataGlobal->AnnualSimulation) {
            DisplayString(state, "ERROR: Cannot force both design-day and annual simulations. Set either '-D' or '-a', but not both.");
            DisplayString(state, errorFollowUp);
            if (eplusRunningViaAPI) {
                return static_cast<int>(ReturnCodes::Failure);
            } else {
                exit(EXIT_FAILURE);
            }
        }

        // Read path from INI file if it exists

        // Check for IDD and IDF files
        if (FileSystem::fileExists(state.files.iniFile.filePath)) {
            auto iniFile = state.files.iniFile.try_open();
            if (!iniFile.good()) {
                DisplayString(state, "ERROR: Could not open file " + iniFile.filePath.string() + " for input (read).");
                if (eplusRunningViaAPI) {
                    return static_cast<int>(ReturnCodes::Failure);
                } else {
                    exit(EXIT_FAILURE);
                }
            }
            state.dataStrGlobals->CurrentWorkingFolder = iniFile.filePath;
            // Relying on compiler to supply full path name here
            // TODO: not sure I understand this block
            // const auto TempIndx = index(state.dataStrGlobals->CurrentWorkingFolder, state.dataStrGlobals->pathChar, true);
            // if (TempIndx == std::string::npos) {
            // state.dataStrGlobals->CurrentWorkingFolder = "";
            //} else {
            // state.dataStrGlobals->CurrentWorkingFolder.erase(TempIndx + 1);
            //}
            //       Get directories from ini file
            std::string programPathStr;
            ReadINIFile(iniFile, "program", "dir", programPathStr);
            state.dataStrGlobals->ProgramPath = fs::path(programPathStr);

            state.dataStrGlobals->inputIddFilePath = state.dataStrGlobals->ProgramPath / "Energy+.idd";
        }

        // Check if specified files exist
        if (!FileSystem::fileExists(state.dataStrGlobals->inputFilePath)) {
            DisplayString(
                state, "ERROR: Could not find input data file: " + FileSystem::getAbsolutePath(state.dataStrGlobals->inputFilePath).string() + ".");
            DisplayString(state, errorFollowUp);
            if (eplusRunningViaAPI) {
                return static_cast<int>(ReturnCodes::Failure);
            } else {
                exit(EXIT_FAILURE);
            }
        }

        if (opt.isSet("-w") && !state.dataGlobal->DDOnlySimulation) {
            if (!FileSystem::fileExists(state.files.inputWeatherFilePath.filePath)) {
                DisplayString(
                    state,
                    "ERROR: Could not find weather file: " + FileSystem::getAbsolutePath(state.files.inputWeatherFilePath.filePath).string() + ".");
                DisplayString(state, errorFollowUp);
                if (eplusRunningViaAPI) {
                    return static_cast<int>(ReturnCodes::Failure);
                } else {
                    exit(EXIT_FAILURE);
                }
            }
        }

        // TODO: might be able to convert epJSON->IDF, run preprocessors, then go back IDF->epJSON

        // Preprocessors (These will likely move to a new file)
        if (runEPMacro) {
            fs::path epMacroPath = (state.dataStrGlobals->exeDirectoryPath / "EPMacro").replace_extension(FileSystem::exeExtension);
            if (!FileSystem::fileExists(epMacroPath)) {
                DisplayString(state, "ERROR: Could not find EPMacro executable: " + FileSystem::getAbsolutePath(epMacroPath).string() + ".");
                if (eplusRunningViaAPI) {
                    return static_cast<int>(ReturnCodes::Failure);
                } else {
                    exit(EXIT_FAILURE);
                }
            }
            std::string epMacroCommand = "\"" + epMacroPath.string() + "\"";
            bool inputFilePathdIn = (FileSystem::getAbsolutePath(state.dataStrGlobals->inputFilePath) == FileSystem::getAbsolutePath("in.imf"));

            if (!inputFilePathdIn) {
                FileSystem::linkFile(state.dataStrGlobals->inputFilePath, "in.imf");
            }
            DisplayString(state, "Running EPMacro...");
            FileSystem::systemCall(epMacroCommand);
            if (!inputFilePathdIn) {
                FileSystem::removeFile("in.imf");
            }
            FileSystem::moveFile("audit.out", outputEpmdetFilePath);
            FileSystem::moveFile("out.idf", outputEpmidfFilePath);
            state.dataStrGlobals->inputFilePath = outputEpmidfFilePath;
        }

        if (runExpandObjects) {
            fs::path expandObjectsPath =
                (state.dataStrGlobals->exeDirectoryPath / fs::path("ExpandObjects")).replace_extension(FileSystem::exeExtension);
            if (!FileSystem::fileExists(expandObjectsPath)) {
                DisplayString(state,
                              "ERROR: Could not find ExpandObjects executable: " + FileSystem::getAbsolutePath(expandObjectsPath).string() + ".");
                if (eplusRunningViaAPI) {
                    return static_cast<int>(ReturnCodes::Failure);
                } else {
                    exit(EXIT_FAILURE);
                }
            }
            std::string expandObjectsCommand = "\"" + expandObjectsPath.string() + "\"";
            bool inputFilePathdIn = (FileSystem::getAbsolutePath(state.dataStrGlobals->inputFilePath) == FileSystem::getAbsolutePath("in.idf"));

            // check if IDD actually exists since ExpandObjects still requires it
            if (!FileSystem::fileExists(state.dataStrGlobals->inputIddFilePath)) {
                DisplayString(state,
                              "ERROR: Could not find input data dictionary: " +
                                  FileSystem::getAbsolutePath(state.dataStrGlobals->inputIddFilePath).string() + ".");
                DisplayString(state, errorFollowUp);
                if (eplusRunningViaAPI) {
                    return static_cast<int>(ReturnCodes::Failure);
                } else {
                    exit(EXIT_FAILURE);
                }
            }

            bool iddFilePathdEnergy =
                (FileSystem::getAbsolutePath(state.dataStrGlobals->inputIddFilePath) == FileSystem::getAbsolutePath("Energy+.idd"));

            if (!inputFilePathdIn) {
                FileSystem::linkFile(state.dataStrGlobals->inputFilePath, "in.idf");
            }
            if (!iddFilePathdEnergy) {
                FileSystem::linkFile(state.dataStrGlobals->inputIddFilePath, "Energy+.idd");
            }

            FileSystem::systemCall(expandObjectsCommand);
            if (!inputFilePathdIn) {
                FileSystem::removeFile("in.idf");
            }
            if (!iddFilePathdEnergy) {
                FileSystem::removeFile("Energy+.idd");
            }

            FileSystem::moveFile("expandedidf.err", outputExperrFilePath);
            if (FileSystem::fileExists("expanded.idf")) {
                FileSystem::moveFile("expanded.idf", outputExpidfFilePath);
                state.dataStrGlobals->inputFilePath = outputExpidfFilePath;
            }
        }

        return static_cast<int>(ReturnCodes::Success);
    }

    // Fix This is Fortranic code that needs to be brought up to C++ style
    //     All the index and len and strip should be eliminated and replaced by string calls only where needed
    //     I/o with std::string should not be pulling in trailing blanks so stripping should not be needed, etc.
    //     Rewinding is a big performance hit and should be avoided if possible
    //     Case-insensitive comparison is much faster than converting strings to upper or lower case
    //     Each strip and case conversion is a heap hit and should be avoided if possible
    void ReadINIFile(InputFile &inputFile,               // Unit number of the opened INI file
                     std::string const &Heading,         // Heading for the parameters ('[heading]')
                     std::string const &KindofParameter, // Kind of parameter to be found (String)
                     std::string &DataOut                // Output from the retrieval
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine reads the .ini file and retrieves
        // the path names for the files from it.

        // METHODOLOGY EMPLOYED:
        // Duplicate the kind of reading the Windows "GetINISetting" would
        // do.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace EnergyPlus;
        using namespace DataStringGlobals;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        std::string Param;
        std::string::size_type ILB;
        std::string::size_type IRB;
        std::string::size_type IEQ;
        std::string::size_type IPAR;
        std::string::size_type IPOS;
        std::string::size_type ILEN;

        // Formats

        DataOut.clear();

        // I tried ADJUSTL(TRIM(KindofParameter)) and got an internal compiler error

        Param = KindofParameter;
        strip(Param);
        ILEN = len(Param);
        inputFile.rewind();
        bool Found = false;
        bool NewHeading = false;

        while (inputFile.good() && !Found) {
            const auto readResult = inputFile.readLine();

            if (readResult.eof) {
                break;
            }

            if (readResult.data.empty()) {
                continue;
            } // Ignore Blank Lines

            std::string LINEOut;
            ConvertCaseToLower(readResult.data, LINEOut); // Turn line into lower case
            //        LINE=LINEOut

            if (!has(LINEOut, Heading)) continue;

            //                                  See if [ and ] are on line
            ILB = index(LINEOut, '[');
            IRB = index(LINEOut, ']');
            if (ILB == std::string::npos && IRB == std::string::npos) continue;
            if (!has(LINEOut, '[' + Heading + ']')) continue; // Must be really correct heading line

            //                                  Heading line found, now looking for Kind
            while (inputFile.good() && !NewHeading) {
                const auto innerReadResult = inputFile.readLine();
                if (innerReadResult.eof) {
                    break;
                }

                auto line = innerReadResult.data;
                strip(line);

                if (line.empty()) continue; // Ignore Blank Lines

                ConvertCaseToLower(line, LINEOut); // Turn line into lower case
                //         LINE=LINEOut

                ILB = index(LINEOut, '[');
                IRB = index(LINEOut, ']');
                NewHeading = (ILB != std::string::npos && IRB != std::string::npos);

                //                                  Should be a parameter line
                //                                  KindofParameter = string
                IEQ = index(LINEOut, '=');
                IPAR = index(LINEOut, Param);
                if (IEQ == std::string::npos) continue;
                if (IPAR == std::string::npos) continue;
                if (IPAR != 0) continue;
                if (!has(LINEOut, Param + '=')) continue; // needs to be param=

                //                                  = found and parameter found.
                if (IPAR > IEQ) continue;

                //                                  parameter = found
                //                                  Set output string to start with non-blank character

                DataOut = stripped(line.substr(IEQ + 1));
                Found = true;
                break;
            }
        }

        if (Param == "dir") {
            IPOS = len(DataOut);
            if (IPOS != 0) {
                // Non-blank make sure last position is valid path character
                //  (Set in DataStringGlobals)

                if (DataOut[IPOS - 1] != pathChar) {
                    DataOut += pathChar;
                }
            }
        }
    }

    int runReadVarsESO(EnergyPlusData &state)
    {
        fs::path readVarsPath = (state.dataStrGlobals->exeDirectoryPath / "ReadVarsESO").replace_extension(FileSystem::exeExtension);

        if (!FileSystem::fileExists(readVarsPath)) {
            readVarsPath = (state.dataStrGlobals->exeDirectoryPath / "PostProcess" / "ReadVarsESO").replace_extension(FileSystem::exeExtension);
            if (!FileSystem::fileExists(readVarsPath)) {
                // should report the error differently if the user is calling into E+ through EXE or DLL
                if (state.dataGlobal->eplusRunningViaAPI) {
                    DisplayString(
                        state,
                        "ERROR: Could not find ReadVarsESO executable.  When calling through C API, make sure to call setEnergyPlusRootDirectory");
                } else {
                    DisplayString(state, "ERROR: Could not find ReadVarsESO executable: " + FileSystem::getAbsolutePath(readVarsPath).string() + ".");
                }
                return static_cast<int>(ReturnCodes::Failure);
            }
        }

        fs::path const RVIfile = (state.dataStrGlobals->inputDirPath / state.dataStrGlobals->inputFilePathNameOnly).replace_extension(".rvi");
        fs::path const MVIfile = (state.dataStrGlobals->inputDirPath / state.dataStrGlobals->inputFilePathNameOnly).replace_extension(".mvi");

        const auto rviFileExists = FileSystem::fileExists(RVIfile);
        if (!rviFileExists) {
            std::ofstream ofs{RVIfile};
            if (!ofs.good()) {
                ShowFatalError(state, "EnergyPlus: Could not open file \"" + RVIfile.string() + "\" for output (write).");
            } else {
                ofs << state.files.eso.filePath.string() << '\n';
                ofs << state.files.csv.filePath.string() << '\n';
            }
        }

        const auto mviFileExists = FileSystem::fileExists(MVIfile);
        if (!mviFileExists) {
            std::ofstream ofs{MVIfile};
            if (!ofs.good()) {
                ShowFatalError(state, "EnergyPlus: Could not open file \"" + RVIfile.string() + "\" for output (write).");
            } else {
                ofs << state.files.mtr.filePath.string() << '\n';
                ofs << state.files.mtr_csv.filePath.string() << '\n';
            }
        }

        // We quote the paths in case we have spaces
        // "/Path/to/ReadVarEso" "/Path/to/folder with spaces/file.rvi" unlimited
        std::string const readVarsRviCommand = "\"" + readVarsPath.string() + "\" \"" + RVIfile.string() + "\" unlimited";
        std::string const readVarsMviCommand = "\"" + readVarsPath.string() + "\" \"" + MVIfile.string() + "\" unlimited";

        // systemCall will be responsible to handle to above command on Windows versus Unix
        FileSystem::systemCall(readVarsRviCommand);
        FileSystem::systemCall(readVarsMviCommand);

        if (!rviFileExists) {
            FileSystem::removeFile(RVIfile);
        }

        if (!mviFileExists) {
            FileSystem::removeFile(MVIfile);
        }

        FileSystem::moveFile("readvars.audit", state.dataStrGlobals->outputRvauditFilePath);
        return static_cast<int>(ReturnCodes::Success);
    }

} // namespace CommandLineInterface
} // namespace EnergyPlus
