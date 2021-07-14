// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#include <memory>
#include <string>
#ifdef _OPENMP
#include <omp.h>
#endif

#include "EnergyPlus/DataStringGlobals.hh"
#include "EnergyPlus/FileSystem.hh"
#include "EnergyPlus/InputProcessing/EmbeddedEpJSONSchema.hh"
#include "EnergyPlus/InputProcessing/IdfParser.hh"
#include "EnergyPlus/InputProcessing/InputValidation.hh"
#include <ezOptionParser.hpp>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

enum class OutputTypes
{
    Default,
    IDF,
    epJSON,
    CBOR,
    MsgPack,
    UBJSON,
    BSON
};

template <typename T> void displayMessage(T t)
{
    std::cout << t << '\n';
}

template <typename T, typename... Args> void displayMessage(T t, Args... args) // recursive variadic function
{
    std::cout << t;
    displayMessage(args...);
}

bool checkVersionMatch(json const &epJSON)
{
    auto it = epJSON.find("Version");
    if (it != epJSON.end()) {
        for (auto const &version : it.value()) {
            std::string v = version["version_identifier"];
            if (v.empty()) {
                displayMessage("Input errors occurred and version ID was left blank, verify file version");
            } else {
                std::string::size_type const lenVer(len(EnergyPlus::DataStringGlobals::MatchVersion));
                int Which;
                if ((lenVer > 0) && (EnergyPlus::DataStringGlobals::MatchVersion[lenVer - 1] == '0')) {
                    Which = static_cast<int>(index(v.substr(0, lenVer - 2), EnergyPlus::DataStringGlobals::MatchVersion.substr(0, lenVer - 2)));
                } else {
                    Which = static_cast<int>(index(v, EnergyPlus::DataStringGlobals::MatchVersion));
                }
                if (Which != 0) {
                    displayMessage("Version: in IDF=\"" + v + "\" not the same as expected=\"" + EnergyPlus::DataStringGlobals::MatchVersion + "\"");
                    return false;
                }
            }
        }
    }
    return true;
}

bool processErrors(std::unique_ptr<IdfParser> const &idf_parser, std::unique_ptr<Validation> const &validation)
{
    auto const idf_parser_errors = idf_parser->errors();
    auto const idf_parser_warnings = idf_parser->warnings();

    auto const validation_errors = validation->errors();
    auto const validation_warnings = validation->warnings();

    for (auto const &error : idf_parser_errors) {
        displayMessage(error);
    }
    for (auto const &warning : idf_parser_warnings) {
        displayMessage(warning);
    }
    for (auto const &error : validation_errors) {
        displayMessage(error);
    }
    for (auto const &warning : validation_warnings) {
        displayMessage(warning);
    }

    bool has_errors = validation->hasErrors() || idf_parser->hasErrors();

    return has_errors;
}

void cleanEPJSON(json &epjson)
{
    if (epjson.type() == json::value_t::object) {
        epjson.erase("idf_order");
        epjson.erase("idf_max_fields");
        epjson.erase("idf_max_extensible_fields");
        for (auto it = epjson.begin(); it != epjson.end(); ++it) {
            cleanEPJSON(epjson[it.key()]);
        }
    }
}

bool processInput(std::string const &inputFilePath, json const &schema, OutputTypes outputType, fs::path outputDirPath, std::string &outputTypeStr)
{
    auto validation(std::unique_ptr<Validation>(new Validation(&schema)));
    auto idf_parser(std::unique_ptr<IdfParser>(new IdfParser()));
    json epJSON;

    auto const inputFilePathNameOnly = EnergyPlus::FileSystem::removeFileExtension(EnergyPlus::FileSystem::getFileName(inputFilePath));
    auto const inputDirPath = EnergyPlus::FileSystem::getParentDirectoryPath(inputFilePath);

    if (outputDirPath.empty()) {
        outputDirPath = inputDirPath;
    }

    auto inputFileExt = EnergyPlus::FileSystem::getFileExtension(inputFilePath).string();
    std::transform(inputFileExt.begin(), inputFileExt.end(), inputFileExt.begin(), ::toupper);

    bool isEpJSON = true;
    bool isCBOR = false;
    bool isMsgPack = false;
    bool isUBJSON = false;
    bool isBSON = false;

    if (inputFileExt == "EPJSON" || inputFileExt == "JSON") {
        isEpJSON = true;
        if (outputType == OutputTypes::epJSON) {
            displayMessage("Same output format as input format requested (epJSON). Skipping conversion and moving to next file.");
            return false;
        }
    } else if (inputFileExt == "IDF" || inputFileExt == "IMF") {
        isEpJSON = false;
        if (outputType == OutputTypes::IDF) {
            displayMessage("Same output format as input format requested (IDF). Skipping conversion and moving to next file.");
            return false;
        }
    } else if (inputFileExt == "CBOR") {
        isEpJSON = true;
        isCBOR = true;
        if (outputType == OutputTypes::CBOR) {
            displayMessage("Same output format as input format requested (CBOR). Skipping conversion and moving to next file.");
            return false;
        }
    } else if (inputFileExt == "MSGPACK") {
        isEpJSON = true;
        isMsgPack = true;
        if (outputType == OutputTypes::MsgPack) {
            displayMessage("Same output format as input format requested (MsgPack). Skipping conversion and moving to next file.");
            return false;
        }
    } else if (inputFileExt == "UBJSON") {
        isEpJSON = true;
        isUBJSON = true;
        if (outputType == OutputTypes::UBJSON) {
            displayMessage("Same output format as input format requested (UBJSON). Skipping conversion and moving to next file.");
            return false;
        }
    } else if (inputFileExt == "BSON") {
        isEpJSON = true;
        isBSON = true;
        if (outputType == OutputTypes::BSON) {
            displayMessage("Same output format as input format requested (BSON). Skipping conversion and moving to next file.");
            return false;
        }
    } else {
        displayMessage("ERROR: Input file must have IDF, IMF, or epJSON extension.");
        return false;
    }

    std::ifstream input_stream(inputFilePath, std::ifstream::in | std::ifstream::binary);
    if (!input_stream.is_open()) {
        displayMessage("Input file path " + inputFilePath + " not found");
        return false;
    }

#ifdef _WIN32
    std::string const NL("\r\n"); // Platform newline
#else
    std::string const NL("\n"); // Platform newline
#endif

    try {
        if (!isEpJSON) {
            std::string input_file;
            std::string line;
            while (std::getline(input_stream, line)) {
                input_file.append(line + NL);
            }
            if (input_file.empty()) {
                displayMessage("Failed to read input file: " + inputFilePath);
                return false;
            }

            bool success = true;
            epJSON = idf_parser->decode(input_file, schema, success);
            cleanEPJSON(epJSON);
        } else if (isCBOR) {
            epJSON = json::from_cbor(input_stream);
        } else if (isMsgPack) {
            epJSON = json::from_msgpack(input_stream);
        } else if (isUBJSON) {
            epJSON = json::from_ubjson(input_stream);
        } else if (isBSON) {
            epJSON = json::from_bson(input_stream);
        } else {
            epJSON = json::parse(input_stream);
        }
    } catch (const std::exception &e) {
        displayMessage(e.what());
        displayMessage("Errors occurred when processing input file. Preceding condition(s) cause termination.");
        return false;
    }

    bool is_valid = validation->validate(epJSON);
    bool hasErrors = processErrors(idf_parser, validation);
    bool versionMatch = checkVersionMatch(epJSON);

    if (!is_valid || hasErrors) {
        displayMessage("Errors occurred when validating input file. Preceding condition(s) cause termination.");
        return false;
    }

    if (isEpJSON && !versionMatch) {
        displayMessage("Skipping conversion of input file to IDF due to mismatched Version.");
        return false;
    }

    fs::path fileNameWithoutExtension = outputDirPath / inputFilePathNameOnly;
    if ((outputType == OutputTypes::Default || outputType == OutputTypes::IDF) && isEpJSON) {
        auto const input_file = idf_parser->encode(epJSON, schema);
        fs::path convertedEpJSON =
            EnergyPlus::FileSystem::makeNativePath(EnergyPlus::FileSystem::replaceFileExtension(fileNameWithoutExtension, ".idf"));
        std::ofstream convertedFS(convertedEpJSON, std::ofstream::out | std::ofstream::binary);
        convertedFS << input_file << std::endl;
        outputTypeStr = "IDF";
    } else if ((outputType == OutputTypes::Default || outputType == OutputTypes::epJSON) && !isEpJSON) {
        auto const input_file = epJSON.dump(4, ' ', false, json::error_handler_t::replace);
        fs::path convertedIDF =
            EnergyPlus::FileSystem::makeNativePath(EnergyPlus::FileSystem::replaceFileExtension(fileNameWithoutExtension, ".epJSON"));
        std::ofstream convertedFS(convertedIDF, std::ofstream::out | std::ofstream::binary);
        convertedFS << input_file << std::endl;
        outputTypeStr = "EPJSON";
    } else if (outputType == OutputTypes::CBOR) {
        fs::path convertedCBOR =
            EnergyPlus::FileSystem::makeNativePath(EnergyPlus::FileSystem::replaceFileExtension(fileNameWithoutExtension, ".cbor"));
        std::ofstream convertedFS(convertedCBOR, std::ofstream::out | std::ofstream::binary);
        json::to_cbor(epJSON, convertedFS);
    } else if (outputType == OutputTypes::MsgPack) {
        fs::path convertedMsgPack =
            EnergyPlus::FileSystem::makeNativePath(EnergyPlus::FileSystem::replaceFileExtension(fileNameWithoutExtension, ".msgpack"));
        std::ofstream convertedFS(convertedMsgPack, std::ofstream::out | std::ofstream::binary);
        json::to_msgpack(epJSON, convertedFS);
    } else if (outputType == OutputTypes::UBJSON) {
        fs::path convertedUBJSON =
            EnergyPlus::FileSystem::makeNativePath(EnergyPlus::FileSystem::replaceFileExtension(fileNameWithoutExtension, ".ubjson"));
        std::ofstream convertedFS(convertedUBJSON, std::ofstream::out | std::ofstream::binary);
        json::to_ubjson(epJSON, convertedFS);
    } else if (outputType == OutputTypes::BSON) {
        fs::path convertedBSON =
            EnergyPlus::FileSystem::makeNativePath(EnergyPlus::FileSystem::replaceFileExtension(fileNameWithoutExtension, ".bson"));
        std::ofstream convertedFS(convertedBSON, std::ofstream::out | std::ofstream::binary);
        json::to_bson(epJSON, convertedFS);
    } else {
        return false;
    }
    return true;
}

std::vector<std::string> parse_input_paths(std::string const &input_paths_file)
{
    std::ifstream input_paths_stream(input_paths_file);
    if (!input_paths_stream.is_open()) {
        displayMessage("Could not open file: " + input_paths_file);
        return std::vector<std::string>();
    }
    std::vector<std::string> input_paths;
    std::string line;
    while (std::getline(input_paths_stream, line)) {
        if (line.empty()) continue;
        input_paths.emplace_back(line);
    }
    return input_paths;
}

int main(int argc, const char *argv[])
{

    ez::ezOptionParser opt;

    opt.overview = "Run input file conversion tool";
    opt.syntax = "ConvertInputFormat [OPTIONS] input_file [input_file ..]";
    opt.example = "ConvertInputFormat in.idf\n\n";

    opt.add("1",                 // Default.
            false,               // Required?
            1,                   // Number of args expected.
            0,                   // Delimiter if expecting multiple args.
            "Number of threads", // Help description.
            "-j"                 // Flag token.
    );

    opt.add("",                                                                  // Default.
            false,                                                               // Required?
            1,                                                                   // Number of args expected.
            0,                                                                   // Delimiter if expecting multiple args.
            "Text file with list of input files to convert (newline delimited)", // Help description.
            "-i",                                                                // Flag token.
            "--input"                                                            // Flag token.
    );

    opt.add("",                                                           // Default.
            false,                                                        // Required?
            1,                                                            // Number of args expected.
            0,                                                            // Delimiter if expecting multiple args.
            "Output directory. Will use input file location by default.", // Help description.
            "-o",                                                         // Flag token.
            "--output"                                                    // Flag token.
    );

    const char *validOptions[] = {"default", "idf", "epjson", "json", "cbor", "msgpack", "ubjson", "bson"};
    auto *outputTypeValidation = new ez::ezOptionValidator(ez::ezOptionValidator::T, ez::ezOptionValidator::IN, validOptions, 8, true);

    opt.add("default", // Default.
            0,         // Required?
            1,         // Number of args expected.
            0,         // Delimiter if expecting multiple args.
            "Output format.\nDefault means IDF->epJSON or epJSON->IDF\nSelect one (case "
            "insensitive):\ndefault,idf,epjson,json,cbor,msgpack,ubjson,bson", // Help description.
            "-f",                                                              // Flag token.
            "--format",                                                        // Flag token.
            outputTypeValidation);

    opt.add("", 0, 0, 0, "Display version information", "-v", "--version");

    opt.add("",                            // Default.
            false,                         // Required?
            0,                             // Number of args expected.
            0,                             // Delimiter if expecting multiple args.
            "Display usage instructions.", // Help description.
            "-h",                          // Flag token.
            "-help",                       // Flag token.
            "--help",                      // Flag token.
            "--usage"                      // Flag token.
    );

    opt.parse(argc, argv);

    std::string usage;
    opt.getUsage(usage);

    // Process standard arguments
    if (opt.isSet("-h")) {
        displayMessage(usage);
        exit(EXIT_SUCCESS);
    }

    if (opt.isSet("-v")) {
        displayMessage(EnergyPlus::DataStringGlobals::VerString);
        exit(EXIT_SUCCESS);
    }

    int number_of_threads = 1;
    if (opt.isSet("-j")) {
        opt.get("-j")->getInt(number_of_threads);
#ifndef _OPENMP
        displayMessage("ConvertInputFormat is not compiled with OpenMP. Only running on 1 thread, not requested ", number_of_threads, " threads.");
#endif
    }

    std::string output_directory;
    if (opt.isSet("-o")) {
        opt.get("-o")->getString(output_directory);
        if (output_directory.back() != EnergyPlus::DataStringGlobals::pathChar) {
            output_directory.push_back(EnergyPlus::DataStringGlobals::pathChar);
        }
        EnergyPlus::FileSystem::makeDirectory(output_directory);
    }

    std::vector<std::string> files;
    if (opt.isSet("-i")) {
        std::string input_paths_file;
        opt.get("-i")->getString(input_paths_file);
        files = parse_input_paths(input_paths_file);
    }

    std::string outputTypeStr;
    OutputTypes outputType = OutputTypes::Default;
    if (opt.isSet("-f")) {
        opt.get("-f")->getString(outputTypeStr);
        std::transform(outputTypeStr.begin(), outputTypeStr.end(), outputTypeStr.begin(), ::toupper);
        if (outputTypeStr == "EPJSON" || outputTypeStr == "JSON") {
            outputType = OutputTypes::epJSON;
            outputTypeStr = "EPJSON";
        } else if (outputTypeStr == "IDF") {
            outputType = OutputTypes::IDF;
        } else if (outputTypeStr == "CBOR") {
            outputType = OutputTypes::CBOR;
            displayMessage("CBOR input format is experimental.");
        } else if (outputTypeStr == "MSGPACK") {
            outputType = OutputTypes::MsgPack;
            displayMessage("MsgPack input format is experimental.");
        } else if (outputTypeStr == "UBJSON") {
            outputType = OutputTypes::UBJSON;
            displayMessage("UBJSON input format is experimental.");
        } else if (outputTypeStr == "BSON") {
            outputType = OutputTypes::BSON;
            displayMessage("BSON input format is experimental.");
        } else {
            displayMessage("ERROR: Output type must be IDF, epJSON, CBOR, MsgPack, UBJSON, or BSON.");
            return 1;
        }
    }

    if (!opt.lastArgs.empty()) {
        for (auto const &lastArg : opt.lastArgs) {
            files.emplace_back(*lastArg);
        }
    } else if (opt.firstArgs.size() > 1) {
        for (std::size_t i = 1; i < opt.firstArgs.size(); ++i) {
            files.emplace_back(*opt.firstArgs[i]);
        }
    }

    std::vector<std::string> badOptions;
    if (!opt.gotRequired(badOptions)) {
        for (auto const &badOption : badOptions) {
            std::cerr << "ERROR: Missing required option " << badOption << ".\n\n";
        }
        displayMessage(usage);
        return 1;
    }

    if (!opt.gotExpected(badOptions)) {
        for (auto const &badOption : badOptions) {
            std::cerr << "ERROR: Got unexpected number of arguments for option " << badOption << ".\n\n";
        }
        displayMessage(usage);
        return 1;
    }

    std::vector<std::string> badArgs;
    if (!opt.gotValid(badOptions, badArgs)) {
        for (std::size_t i = 0; i < badOptions.size(); ++i) {
            std::cerr << "ERROR: Got invalid argument \"" << badArgs[i] << "\" for option " << badOptions[i] << ".\n\n";
        }
        return 1;
    }

    auto const embeddedEpJSONSchema = EnergyPlus::EmbeddedEpJSONSchema::embeddedEpJSONSchema();
    auto schema = json::from_cbor(embeddedEpJSONSchema.first, embeddedEpJSONSchema.second);

    int number_files = static_cast<int>(files.size());
    std::size_t fileCount = 0;

#ifdef _OPENMP
    omp_set_num_threads(number_of_threads);
#endif

#ifdef _OPENMP
#pragma omp parallel default(none) shared(files, number_files, fileCount, schema, outputType, outputTypeStr, output_directory)
    {
#pragma omp for
        for (int i = 0; i < number_files; ++i) {
            bool successful = processInput(files[i], schema, outputType, output_directory, outputTypeStr);
#pragma omp atomic
            ++fileCount;
            if (successful) {
                displayMessage("Input file converted to ", outputTypeStr, " successfully | ", fileCount, "/", number_files, " | ", files[i]);
            } else {
                displayMessage("Input file conversion failed: | ", fileCount, "/", number_files, " | ", files[i]);
            }
        }
    }
#else
    for (auto const &file : files) {
        bool successful = processInput(file, schema, outputType, output_directory, outputTypeStr);
        ++fileCount;
        if (successful) {
            displayMessage("Input file converted to ", outputTypeStr, " successfully | ", fileCount, "/", number_files, " | ", file);
        } else {
            displayMessage("Input file conversion failed: | ", fileCount, "/", number_files, " | ", file);
        }
    }
#endif

    return 0;
}
