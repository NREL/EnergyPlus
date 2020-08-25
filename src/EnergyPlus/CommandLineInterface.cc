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

// CLI Headers
#include <ezOptionParser.hpp>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// Project headers
#include <EnergyPlus/CommandLineInterface.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace CommandLineInterface {

    using namespace DataGlobals;
    using namespace DataStringGlobals;
    using namespace FileSystem;
    using namespace OutputProcessor;
    using namespace ez;

    int ProcessArgs(EnergyPlusData &state, int argc, const char *argv[])
    {
        typedef std::string::size_type size_type;

        // Expand long-name options using "=" sign into two arguments
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

        opt.overview = VerString + "\nPythonLinkage: " + PluginManagement::pythonStringForUsage();

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

        opt.add("", 0, 0, 0, "Display version information", "-v", "--version");

        opt.add("in.epw", 0, 1, 0, "Weather file path (default: in.epw in current directory)", "-w", "--weather");

        opt.add("", 0, 0, 0, "Run ExpandObjects prior to simulation", "-x", "--expandobjects");

        opt.example = "energyplus -w weather.epw -r input.idf";

        std::string errorFollowUp = "Type 'energyplus --help' for usage.";

        // Parse arguments
        opt.parse(argCount, &cStrArgs[0]);

        // print arguments parsed (useful for debugging)
        /*std::string pretty;
        opt.prettyPrint(pretty);
        std::cout << pretty << std::endl;*/

        std::string usage;
        opt.getUsage(usage);

        // Set path of EnergyPlus program path
        exeDirectory = getParentDirectoryPath(getAbsolutePath(getProgramPath()));

        opt.get("-w")->getString(state.files.inputWeatherFileName.fileName);

        opt.get("-i")->getString(inputIddFileName);

        if (!opt.isSet("-i") && !legacyMode) inputIddFileName = exeDirectory + inputIddFileName;

        opt.get("-d")->getString(outDirPathName);

        runReadVars = opt.isSet("-r");

        DDOnlySimulation = opt.isSet("-D");

        state.dataGlobals.AnnualSimulation = opt.isSet("-a");

        outputEpJSONConversion = opt.isSet("-c");

        outputEpJSONConversionOnly = opt.isSet("--convert-only");

        // Process standard arguments
        if (opt.isSet("-h")) {
            DisplayString(usage);
            exit(EXIT_SUCCESS);
        }

        if (opt.isSet("-v")) {
            DisplayString(VerString);
            exit(EXIT_SUCCESS);
        }

        if (opt.lastArgs.size() == 1) {
            for (size_type i = 0; i < opt.lastArgs.size(); ++i) {
                std::string const &arg(*opt.lastArgs[i]);
                inputFileName = arg;
            }
        }
        if (opt.lastArgs.size() == 0) inputFileName = "in.idf";

        // Convert all paths to native paths
        makeNativePath(inputFileName);
        makeNativePath(state.files.inputWeatherFileName.fileName);
        makeNativePath(inputIddFileName);
        makeNativePath(outDirPathName);

        std::vector<std::string> badOptions;
        if (opt.lastArgs.size() > 1u) {
            bool invalidOptionFound = false;
            for (size_type i = 0; i < opt.lastArgs.size(); ++i) {
                std::string const &arg(*opt.lastArgs[i]);
                if (arg.substr(0, 1) == "-") {
                    invalidOptionFound = true;
                    DisplayString("ERROR: Invalid option: " + arg);
                }
            }
            if (invalidOptionFound) {
                DisplayString(errorFollowUp);
                exit(EXIT_FAILURE);
            } else {
                DisplayString("ERROR: Multiple input files specified:");
                for (size_type i = 0; i < opt.lastArgs.size(); ++i) {
                    std::string const &arg(*opt.lastArgs[i]);
                    DisplayString("  Input file #" + std::to_string(i + 1) + ": " + arg);
                }
                DisplayString(errorFollowUp);
                exit(EXIT_FAILURE);
            }
        }

        inputFileNameOnly = removeFileExtension(getFileName(inputFileName));
        inputDirPathName = getParentDirectoryPath(inputFileName);

        auto inputFileExt = getFileExtension(inputFileName);
        std::transform(inputFileExt.begin(), inputFileExt.end(), inputFileExt.begin(), ::toupper);

        // TODO: figure out better logic for determining input file type
        if (inputFileExt == "EPJSON" || inputFileExt == "JSON") {
            isEpJSON = true;
        } else if (inputFileExt == "IDF" || inputFileExt == "IMF") {
            isEpJSON = false;
        } else if (inputFileExt == "CBOR") {
            isEpJSON = true;
            isCBOR = true;
            DisplayString("CBOR input format is experimental and unsupported.");
        } else if (inputFileExt == "MSGPACK") {
            isEpJSON = true;
            isMsgPack = true;
            DisplayString("MsgPack input format is experimental and unsupported.");
        } else if (inputFileExt == "UBJSON") {
            isEpJSON = true;
            isUBJSON = true;
            DisplayString("UBJSON input format is experimental and unsupported.");
        } else if (inputFileExt == "BSON") {
            isEpJSON = true;
            isBSON = true;
            DisplayString("BSON input format is experimental and unsupported.");
        } else {
            DisplayString("ERROR: Input file must have IDF, IMF, or epJSON extension.");
            exit(EXIT_FAILURE);
        }

        std::string weatherFilePathWithoutExtension = removeFileExtension(state.files.inputWeatherFileName.fileName);

        bool runExpandObjects(false);
        bool runEPMacro(false);

        runExpandObjects = opt.isSet("-x");

        runEPMacro = opt.isSet("-m");

        if (opt.isSet("-d")) {
            // Add the trailing path character if necessary
            if (outDirPathName[outDirPathName.size() - 1] != pathChar) {
                outDirPathName += pathChar;
            }

            // Create directory if it doesn't already exist
            makeDirectory(outDirPathName);
        }

        outputDirPathName = outDirPathName;

        // File naming scheme
        std::string outputFilePrefix;
        if (opt.isSet("-p")) {
            std::string prefixOutName;
            opt.get("-p")->getString(prefixOutName);
            makeNativePath(prefixOutName);
            outputFilePrefix = outDirPathName + prefixOutName;
        } else {
            outputFilePrefix = outDirPathName + "eplus";
        }

        std::string suffixType;
        opt.get("-s")->getString(suffixType);

        std::string outputEpmdetFileName;
        std::string outputEpmidfFileName;

        std::string outputExpidfFileName;
        std::string outputExperrFileName;

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

        if (suffixType == "L" || suffixType == "l") {

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

        } else if (suffixType == "D" || suffixType == "d") {

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

        } else if (suffixType == "C" || suffixType == "c") {

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
            DisplayString("ERROR: Unrecognized argument for output suffix style: " + suffixType);
            DisplayString(errorFollowUp);
            exit(EXIT_FAILURE);
        }

        // EnergyPlus files
        state.files.audit.fileName = outputFilePrefix + normalSuffix + ".audit";
        state.files.bnd.fileName = outputFilePrefix + normalSuffix + ".bnd";
        state.files.dxf.fileName = outputFilePrefix + normalSuffix + ".dxf";
        state.files.eio.fileName = outputFilePrefix + normalSuffix + ".eio";
        state.files.endFile.fileName = outputFilePrefix + normalSuffix + ".end";
        state.files.outputErrFileName = outputFilePrefix + normalSuffix + ".err";
        state.files.eso.fileName = outputFilePrefix + normalSuffix + ".eso";

        state.files.json.outputJsonFileName = outputFilePrefix + normalSuffix + ".json";
        state.files.json.outputTSZoneJsonFileName = outputFilePrefix + normalSuffix + "_detailed_zone.json";
        state.files.json.outputTSHvacJsonFileName = outputFilePrefix + normalSuffix + "_detailed_HVAC.json";
        state.files.json.outputTSJsonFileName = outputFilePrefix + normalSuffix + "_timestep.json";
        state.files.json.outputYRJsonFileName = outputFilePrefix + normalSuffix + "_yearly.json";
        state.files.json.outputMNJsonFileName = outputFilePrefix + normalSuffix + "_monthly.json";
        state.files.json.outputDYJsonFileName = outputFilePrefix + normalSuffix + "_daily.json";
        state.files.json.outputHRJsonFileName = outputFilePrefix + normalSuffix + "_hourly.json";
        state.files.json.outputSMJsonFileName = outputFilePrefix + normalSuffix + "_runperiod.json";
        state.files.json.outputCborFileName = outputFilePrefix + normalSuffix + ".cbor";
        state.files.json.outputTSZoneCborFileName = outputFilePrefix + normalSuffix + "_detailed_zone.cbor";
        state.files.json.outputTSHvacCborFileName = outputFilePrefix + normalSuffix + "_detailed_HVAC.cbor";
        state.files.json.outputTSCborFileName = outputFilePrefix + normalSuffix + "_timestep.cbor";
        state.files.json.outputYRCborFileName = outputFilePrefix + normalSuffix + "_yearly.cbor";
        state.files.json.outputMNCborFileName = outputFilePrefix + normalSuffix + "_monthly.cbor";
        state.files.json.outputDYCborFileName = outputFilePrefix + normalSuffix + "_daily.cbor";
        state.files.json.outputHRCborFileName = outputFilePrefix + normalSuffix + "_hourly.cbor";
        state.files.json.outputSMCborFileName = outputFilePrefix + normalSuffix + "_runperiod.cbor";
        state.files.json.outputMsgPackFileName = outputFilePrefix + normalSuffix + ".msgpack";
        state.files.json.outputTSZoneMsgPackFileName = outputFilePrefix + normalSuffix + "_detailed_zone.msgpack";
        state.files.json.outputTSHvacMsgPackFileName = outputFilePrefix + normalSuffix + "_detailed_HVAC.msgpack";
        state.files.json.outputTSMsgPackFileName = outputFilePrefix + normalSuffix + "_timestep.msgpack";
        state.files.json.outputYRMsgPackFileName = outputFilePrefix + normalSuffix + "_yearly.msgpack";
        state.files.json.outputMNMsgPackFileName = outputFilePrefix + normalSuffix + "_monthly.msgpack";
        state.files.json.outputDYMsgPackFileName = outputFilePrefix + normalSuffix + "_daily.msgpack";
        state.files.json.outputHRMsgPackFileName = outputFilePrefix + normalSuffix + "_hourly.msgpack";
        state.files.json.outputSMMsgPackFileName = outputFilePrefix + normalSuffix + "_runperiod.msgpack";

        state.files.mtd.fileName = outputFilePrefix + normalSuffix + ".mtd";
        state.files.mdd.fileName = outputFilePrefix + normalSuffix + ".mdd";
        state.files.mtr.fileName = outputFilePrefix + normalSuffix + ".mtr";
        state.files.rdd.fileName = outputFilePrefix + normalSuffix + ".rdd";
        outputShdFileName = outputFilePrefix + normalSuffix + ".shd";
        state.files.dfs.fileName = outputFilePrefix + normalSuffix + ".dfs";
        outputGLHEFileName = outputFilePrefix + normalSuffix + ".glhe";
        state.files.edd.fileName = outputFilePrefix + normalSuffix + ".edd";
        outputIperrFileName = outputFilePrefix + normalSuffix + ".iperr";
        state.files.sln.fileName = outputFilePrefix + normalSuffix + ".sln";
        state.files.sci.fileName = outputFilePrefix + normalSuffix + ".sci";
        state.files.wrl.fileName = outputFilePrefix + normalSuffix + ".wrl";
        outputSqlFileName = outputFilePrefix + normalSuffix + ".sql";
        state.files.debug.fileName = outputFilePrefix + normalSuffix + ".dbg";
        outputPerfLogFileName = outputFilePrefix + normalSuffix + "_perflog.csv";
        outputTblCsvFileName = outputFilePrefix + tableSuffix + ".csv";
        outputTblHtmFileName = outputFilePrefix + tableSuffix + ".htm";
        outputTblTabFileName = outputFilePrefix + tableSuffix + ".tab";
        outputTblTxtFileName = outputFilePrefix + tableSuffix + ".txt";
        outputTblXmlFileName = outputFilePrefix + tableSuffix + ".xml";
        state.files.outputMapTabFileName = outputFilePrefix + mapSuffix + ".tab";
        state.files.outputMapCsvFileName = outputFilePrefix + mapSuffix + ".csv";
        state.files.outputMapTxtFileName = outputFilePrefix + mapSuffix + ".txt";
        state.files.outputZszCsvFileName = outputFilePrefix + zszSuffix + ".csv";
        state.files.outputZszTabFileName = outputFilePrefix + zszSuffix + ".tab";
        state.files.outputZszTxtFileName = outputFilePrefix + zszSuffix + ".txt";
        state.files.outputSszCsvFileName = outputFilePrefix + sszSuffix + ".csv";
        state.files.outputSszTabFileName = outputFilePrefix + sszSuffix + ".tab";
        state.files.outputSszTxtFileName = outputFilePrefix + sszSuffix + ".txt";
        outputAdsFileName = outputFilePrefix + adsSuffix + ".out";
        state.files.shade.fileName = outputFilePrefix + shdSuffix + ".csv";
        if (suffixType == "L" || suffixType == "l") {
            outputSqliteErrFileName = outDirPathName + sqliteSuffix + ".err";
        } else {
            outputSqliteErrFileName = outputFilePrefix + sqliteSuffix + ".err";
        }
        state.files.screenCsv.fileName = outputFilePrefix + screenSuffix + ".csv";
        state.files.delightIn.fileName = "eplusout.delightin";
        outputDelightOutFileName = "eplusout.delightout";
        state.files.iniFile.fileName = "Energy+.ini";
        state.files.inStatFileName.fileName = weatherFilePathWithoutExtension + ".stat";
        eplusADSFileName = inputDirPathName + "eplusADS.inp";

        // Readvars files
        state.files.csv.fileName = outputFilePrefix + normalSuffix + ".csv";
        state.files.mtr_csv.fileName = outputFilePrefix + meterSuffix + ".csv";
        outputRvauditFileName = outputFilePrefix + normalSuffix + ".rvaudit";

        // EPMacro files
        outputEpmdetFileName = outputFilePrefix + normalSuffix + ".epmdet";
        outputEpmidfFileName = outputFilePrefix + normalSuffix + ".epmidf";

        // ExpandObjects files
        outputExpidfFileName = outputFilePrefix + normalSuffix + ".expidf";
        outputExperrFileName = outputFilePrefix + normalSuffix + ".experr";

        // Handle bad options
        if (!opt.gotExpected(badOptions)) {
            for (size_type i = 0; i < badOptions.size(); ++i) {
                DisplayString("ERROR: Unexpected number of arguments for option " + badOptions[i]);
            }
            DisplayString(errorFollowUp);
            exit(EXIT_FAILURE);
        }

        // This is a place holder in case there are required options in the future
        if (!opt.gotRequired(badOptions)) {
            for (size_type i = 0; i < badOptions.size(); ++i) {
                DisplayString("ERROR: Missing required option " + badOptions[i]);
            }
            DisplayString(errorFollowUp);
            exit(EXIT_FAILURE);
        }

        if (opt.firstArgs.size() > 1 || opt.unknownArgs.size() > 0) {
            for (size_type i = 1; i < opt.firstArgs.size(); ++i) {
                std::string const &arg(*opt.firstArgs[i]);
                DisplayString("ERROR: Invalid option: " + arg);
            }
            for (size_type i = 0; i < opt.unknownArgs.size(); ++i) {
                std::string const &arg(*opt.unknownArgs[i]);
                DisplayString("ERROR: Invalid option: " + arg);
            }
            DisplayString(errorFollowUp);
            exit(EXIT_FAILURE);
        }

        // Error for cases where both design-day and annual simulation switches are set
        if (DDOnlySimulation && state.dataGlobals.AnnualSimulation) {
            DisplayString("ERROR: Cannot force both design-day and annual simulations. Set either '-D' or '-a', but not both.");
            DisplayString(errorFollowUp);
            exit(EXIT_FAILURE);
        }

        // Read path from INI file if it exists

        // Check for IDD and IDF files
        if (fileExists(state.files.iniFile.fileName)) {
            auto iniFile = state.files.iniFile.try_open();
            if (!iniFile.good()) {
                DisplayString("ERROR: Could not open file " + iniFile.fileName + " for input (read).");
                exit(EXIT_FAILURE);
            }
            CurrentWorkingFolder = iniFile.fileName;
            // Relying on compiler to supply full path name here
            const auto TempIndx = index(CurrentWorkingFolder, pathChar, true);
            if (TempIndx == std::string::npos) {
                CurrentWorkingFolder = "";
            } else {
                CurrentWorkingFolder.erase(TempIndx + 1);
            }
            //       Get directories from ini file
            ReadINIFile(iniFile, "program", "dir", ProgramPath);

            inputIddFileName = ProgramPath + "Energy+.idd";
        }

        // Check if specified files exist
        if (!fileExists(inputFileName)) {
            DisplayString("ERROR: Could not find input data file: " + getAbsolutePath(inputFileName) + ".");
            DisplayString(errorFollowUp);
            exit(EXIT_FAILURE);
        }

        if (opt.isSet("-w") && !DDOnlySimulation) {
            if (!fileExists(state.files.inputWeatherFileName.fileName)) {
                DisplayString("ERROR: Could not find weather file: " + getAbsolutePath(state.files.inputWeatherFileName.fileName) + ".");
                DisplayString(errorFollowUp);
                exit(EXIT_FAILURE);
            }
        }

        // TODO: might be able to convert epJSON->IDF, run preprocessors, then go back IDF->epJSON

        // Preprocessors (These will likely move to a new file)
        if (runEPMacro) {
            std::string epMacroPath = exeDirectory + "EPMacro" + exeExtension;
            if (!fileExists(epMacroPath)) {
                DisplayString("ERROR: Could not find EPMacro executable: " + getAbsolutePath(epMacroPath) + ".");
                exit(EXIT_FAILURE);
            }
            std::string epMacroCommand = "\"" + epMacroPath + "\"";
            bool inputFileNamedIn = (getAbsolutePath(inputFileName) == getAbsolutePath("in.imf"));

            if (!inputFileNamedIn) linkFile(inputFileName.c_str(), "in.imf");
            DisplayString("Running EPMacro...");
            systemCall(epMacroCommand);
            if (!inputFileNamedIn) removeFile("in.imf");
            moveFile("audit.out", outputEpmdetFileName);
            moveFile("out.idf", outputEpmidfFileName);
            inputFileName = outputEpmidfFileName;
        }

        if (runExpandObjects) {
            std::string expandObjectsPath = exeDirectory + "ExpandObjects" + exeExtension;
            if (!fileExists(expandObjectsPath)) {
                DisplayString("ERROR: Could not find ExpandObjects executable: " + getAbsolutePath(expandObjectsPath) + ".");
                exit(EXIT_FAILURE);
            }
            std::string expandObjectsCommand = "\"" + expandObjectsPath + "\"";
            bool inputFileNamedIn = (getAbsolutePath(inputFileName) == getAbsolutePath("in.idf"));

            // check if IDD actually exists since ExpandObjects still requires it
            if (!fileExists(inputIddFileName)) {
                DisplayString("ERROR: Could not find input data dictionary: " + getAbsolutePath(inputIddFileName) + ".");
                DisplayString(errorFollowUp);
                exit(EXIT_FAILURE);
            }

            bool iddFileNamedEnergy = (getAbsolutePath(inputIddFileName) == getAbsolutePath("Energy+.idd"));

            if (!inputFileNamedIn) linkFile(inputFileName.c_str(), "in.idf");
            if (!iddFileNamedEnergy) linkFile(inputIddFileName, "Energy+.idd");
            systemCall(expandObjectsCommand);
            if (!inputFileNamedIn) removeFile("in.idf");
            if (!iddFileNamedEnergy) removeFile("Energy+.idd");
            moveFile("expandedidf.err", outputExperrFileName);
            if (fileExists("expanded.idf")) {
                moveFile("expanded.idf", outputExpidfFileName);
                inputFileName = outputExpidfFileName;
            }
        }

        return 0;
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
        static ObjexxFCL::gio::Fmt Format_700("(A)");

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

            if (readResult.eof) { break; }

            if (readResult.data.empty()) { continue; } // Ignore Blank Lines

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
                if (innerReadResult.eof) { break; }

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

    int runReadVarsESO(IOFiles &ioFiles)
    {
        std::string readVarsPath = exeDirectory + "ReadVarsESO" + exeExtension;

        if (!fileExists(readVarsPath)) {
            readVarsPath = exeDirectory + "PostProcess" + pathChar + "ReadVarsESO" + exeExtension;
            if (!fileExists(readVarsPath)) {
                DisplayString("ERROR: Could not find ReadVarsESO executable: " + getAbsolutePath(readVarsPath) + ".");
                return EXIT_FAILURE;
            }
        }

        std::string const RVIfile = inputDirPathName + inputFileNameOnly + ".rvi";
        std::string const MVIfile = inputDirPathName + inputFileNameOnly + ".mvi";

        const auto rviFileExists = fileExists(RVIfile);
        if (!rviFileExists) {
            std::ofstream ofs{RVIfile};
            if (!ofs.good()) {
                ShowFatalError("EnergyPlus: Could not open file \"" + RVIfile + "\" for output (write).");
            } else {
                ofs << ioFiles.eso.fileName << '\n';
                ofs << ioFiles.csv.fileName << '\n';
            }
        }

        const auto mviFileExists = fileExists(MVIfile);
        if (!mviFileExists) {
            std::ofstream ofs{MVIfile};
            if (!ofs.good()) {
                ShowFatalError("EnergyPlus: Could not open file \"" + RVIfile + "\" for output (write).");
            } else {
                ofs << ioFiles.mtr.fileName << '\n';
                ofs << ioFiles.mtr_csv.fileName << '\n';
            }
        }

        // We quote the paths in case we have spaces
        // "/Path/to/ReadVarEso" "/Path/to/folder with spaces/file.rvi" unlimited
        std::string const readVarsRviCommand = "\"" + readVarsPath + "\" \"" + RVIfile + "\" unlimited";
        std::string const readVarsMviCommand = "\"" + readVarsPath + "\" \"" + MVIfile + "\" unlimited";

        // systemCall will be responsible to handle to above command on Windows versus Unix
        systemCall(readVarsRviCommand);
        systemCall(readVarsMviCommand);

        if (!rviFileExists) removeFile(RVIfile.c_str());

        if (!mviFileExists) removeFile(MVIfile.c_str());

        moveFile("readvars.audit", outputRvauditFileName);
        return EXIT_SUCCESS;
    }

} // namespace CommandLineInterface
} // namespace EnergyPlus
