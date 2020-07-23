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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"

// A to Z order
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/InputProcessing/IdfParser.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InputProcessing/InputValidation.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/StateManagement.hh>
#include <algorithm>
#include <fstream>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

namespace EnergyPlus {

void EnergyPlusFixture::SetUpTestCase()
{
    EnergyPlus::inputProcessor = InputProcessor::factory();
}

void EnergyPlusFixture::openOutputFiles(OutputFiles &outputFiles)
{
    outputFiles.eio.open_as_stringstream();
    outputFiles.mtr.open_as_stringstream();
    outputFiles.eso.open_as_stringstream();
    outputFiles.audit.open_as_stringstream();
    outputFiles.bnd.open_as_stringstream();
    outputFiles.debug.open_as_stringstream();
    outputFiles.mtd.open_as_stringstream();
    outputFiles.edd.open_as_stringstream();
}

void EnergyPlusFixture::SetUp()
{
    EnergyPlus::clearAllStates(state);

    show_message();

    openOutputFiles(state.outputFiles);

    this->err_stream = std::unique_ptr<std::ostringstream>(new std::ostringstream);
    this->json_stream = std::unique_ptr<std::ostringstream>(new std::ostringstream);

    DataGlobals::err_stream = this->err_stream.get();
    DataGlobals::jsonOutputStreams.json_stream = this->json_stream.get();

    m_cout_buffer = std::unique_ptr<std::ostringstream>(new std::ostringstream);
    m_redirect_cout = std::unique_ptr<RedirectCout>(new RedirectCout(m_cout_buffer));

    m_cerr_buffer = std::unique_ptr<std::ostringstream>(new std::ostringstream);
    m_redirect_cerr = std::unique_ptr<RedirectCerr>(new RedirectCerr(m_cerr_buffer));

    UtilityRoutines::outputErrorHeader = false;

    Psychrometrics::InitializePsychRoutines();
    FluidProperties::InitializeGlycRoutines();
    createCoilSelectionReportObj();
}

void EnergyPlusFixture::TearDown()
{

    {
        IOFlags flags;
        flags.DISPOSE("DELETE");
        state.outputFiles.mtd.del();
        state.outputFiles.eso.del();
        ObjexxFCL::gio::close(DataGlobals::jsonOutputStreams.OutputFileJson, flags);
        ObjexxFCL::gio::close(DataGlobals::OutputStandardError, flags);
        state.outputFiles.eio.del();
        state.outputFiles.debug.del();
        state.outputFiles.zsz.del();
        state.outputFiles.ssz.del();
        state.outputFiles.mtr.del();
        state.outputFiles.bnd.del();
        ObjexxFCL::gio::close(DataGlobals::OutputFileZonePulse, flags);
        state.outputFiles.shade.del();
    }

    clearAllStates(this->state);
}

std::string EnergyPlusFixture::delimited_string(std::vector<std::string> const &strings, std::string const &delimiter)
{
    std::ostringstream compare_text;
    for (auto const &str : strings) {
        compare_text << str << delimiter;
    }
    return compare_text.str();
}

std::vector<std::string> EnergyPlusFixture::read_lines_in_file(std::string const &filePath)
{
    std::ifstream infile(filePath);
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(infile, line)) {
        lines.push_back(line);
    }
    return lines;
}

bool EnergyPlusFixture::compare_json_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->json_stream->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->json_stream->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::compare_eso_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = state.outputFiles.eso.get_output();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) state.outputFiles.eso.open_as_stringstream();
    return are_equal;
}

bool EnergyPlusFixture::compare_eio_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = state.outputFiles.eio.get_output();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) state.outputFiles.eio.open_as_stringstream();
    return are_equal;
}

bool EnergyPlusFixture::compare_mtr_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = state.outputFiles.mtr.get_output();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) state.outputFiles.mtr.open_as_stringstream();
    return are_equal;
}

bool EnergyPlusFixture::compare_err_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->err_stream->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->err_stream->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::compare_cout_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->m_cout_buffer->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->m_cout_buffer->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::compare_cerr_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = this->m_cerr_buffer->str();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) this->m_cerr_buffer->str(std::string());
    return are_equal;
}

bool EnergyPlusFixture::compare_dfs_stream(std::string const &expected_string, bool reset_stream)
{
    auto const stream_str = state.outputFiles.dfs.get_output();
    EXPECT_EQ(expected_string, stream_str);
    bool are_equal = (expected_string == stream_str);
    if (reset_stream) state.outputFiles.dfs.open_as_stringstream();
    return are_equal;
}

bool EnergyPlusFixture::has_json_output(bool reset_stream)
{
    auto const has_output = this->json_stream->str().size() > 0;
    if (reset_stream) this->json_stream->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::has_eso_output(bool reset_stream)
{
    auto const has_output = !state.outputFiles.eso.get_output().empty();
    if (reset_stream) state.outputFiles.eso.open_as_stringstream();
    return has_output;
}

bool EnergyPlusFixture::has_eio_output(bool reset_stream)
{
    auto const has_output = !state.outputFiles.eio.get_output().empty();
    if (reset_stream) state.outputFiles.eio.open_as_stringstream();
    return has_output;
}

bool EnergyPlusFixture::has_mtr_output(bool reset_stream)
{
    auto const has_output = !state.outputFiles.mtr.get_output().empty();
    if (reset_stream) state.outputFiles.mtr.open_as_stringstream();
    return has_output;
}

bool EnergyPlusFixture::has_err_output(bool reset_stream)
{
    auto const has_output = this->err_stream->str().size() > 0;
    if (reset_stream) this->err_stream->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::has_cout_output(bool reset_stream)
{
    auto const has_output = this->m_cout_buffer->str().size() > 0;
    if (reset_stream) this->m_cout_buffer->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::has_cerr_output(bool reset_stream)
{
    auto const has_output = this->m_cerr_buffer->str().size() > 0;
    if (reset_stream) this->m_cerr_buffer->str(std::string());
    return has_output;
}

bool EnergyPlusFixture::has_dfs_output(bool reset_stream)
{
    auto const has_output = !state.outputFiles.dfs.get_output().empty();
    if (reset_stream) state.outputFiles.dfs.open_as_stringstream();
    return has_output;
}

bool EnergyPlusFixture::process_idf(std::string const &idf_snippet, bool use_assertions)
{
    bool success = true;
    inputProcessor->epJSON = inputProcessor->idf_parser->decode(idf_snippet, inputProcessor->schema, success);

    // Add common objects that will trigger a warning if not present
    if (inputProcessor->epJSON.find("Version") == inputProcessor->epJSON.end()) {
        inputProcessor->epJSON["Version"] = {{"", {{"idf_order", 0}, {"version_identifier", DataStringGlobals::MatchVersion}}}};
    }
    if (inputProcessor->epJSON.find("Building") == inputProcessor->epJSON.end()) {
        inputProcessor->epJSON["Building"] = {{"Bldg",
                                               {{"idf_order", 0},
                                                {"north_axis", 0.0},
                                                {"terrain", "Suburbs"},
                                                {"loads_convergence_tolerance_value", 0.04},
                                                {"temperature_convergence_tolerance_value", 0.4000},
                                                {"solar_distribution", "FullExterior"},
                                                {"maximum_number_of_warmup_days", 25},
                                                {"minimum_number_of_warmup_days", 6}}}};
    }
    if (inputProcessor->epJSON.find("GlobalGeometryRules") == inputProcessor->epJSON.end()) {
        inputProcessor->epJSON["GlobalGeometryRules"] = {{"",
                                                          {{"idf_order", 0},
                                                           {"starting_vertex_position", "UpperLeftCorner"},
                                                           {"vertex_entry_direction", "Counterclockwise"},
                                                           {"coordinate_system", "Relative"},
                                                           {"daylighting_reference_point_coordinate_system", "Relative"},
                                                           {"rectangular_surface_coordinate_system", "Relative"}}}};
    }

    int MaxArgs = 0;
    int MaxAlpha = 0;
    int MaxNumeric = 0;
    inputProcessor->getMaxSchemaArgs(MaxArgs, MaxAlpha, MaxNumeric);

    DataIPShortCuts::cAlphaFieldNames.allocate(MaxAlpha);
    DataIPShortCuts::cAlphaArgs.allocate(MaxAlpha);
    DataIPShortCuts::lAlphaFieldBlanks.dimension(MaxAlpha, false);
    DataIPShortCuts::cNumericFieldNames.allocate(MaxNumeric);
    DataIPShortCuts::rNumericArgs.dimension(MaxNumeric, 0.0);
    DataIPShortCuts::lNumericFieldBlanks.dimension(MaxNumeric, false);

    bool is_valid = inputProcessor->validation->validate(inputProcessor->epJSON);
    bool hasErrors = inputProcessor->processErrors();

    inputProcessor->initializeMaps();
    SimulationManager::PostIPProcessing();
    // inputProcessor->state->printErrors();

    bool successful_processing = success && is_valid && !hasErrors;

    if (!successful_processing && use_assertions) {
        EXPECT_TRUE(compare_err_stream(""));
    }

    return successful_processing;
}

bool EnergyPlusFixture::process_idd(std::string const &idd, bool &errors_found)
{

    std::unique_ptr<std::istream> idd_stream;
    if (!idd.empty()) {
        idd_stream = std::unique_ptr<std::istringstream>(new std::istringstream(idd));
    } else {
        static auto const exeDirectory = FileSystem::getParentDirectoryPath(FileSystem::getAbsolutePath(FileSystem::getProgramPath()));
        static auto idd_location = exeDirectory + "Energy+.schema.epJSON";
        static auto file_exists = FileSystem::fileExists(idd_location);

        if (!file_exists) {
            // Energy+.schema.epJSON is in parent Products folder instead of Debug/Release/RelWithDebInfo/MinSizeRel folder of exe
            idd_location = FileSystem::getParentDirectoryPath(exeDirectory) + "Energy+.schema.epJSON";
            file_exists = FileSystem::fileExists(idd_location);
        }

        if (!file_exists) {
            EXPECT_TRUE(file_exists) << "Energy+.schema.epJSON does not exist at search location." << std::endl
                                     << "epJSON Schema search location: \"" << idd_location << "\"";
            errors_found = true;
            return errors_found;
        }

        idd_stream = std::unique_ptr<std::ifstream>(new std::ifstream(idd_location, std::ios_base::in | std::ios_base::binary));
    }

    if (!idd_stream->good()) {
        errors_found = true;
        return errors_found;
    }

    inputProcessor->schema = json::parse(*idd_stream);

    return errors_found;
}

bool EnergyPlusFixture::compare_idf(std::string const &EP_UNUSED(name),
                                    int const EP_UNUSED(num_alphas),
                                    int const EP_UNUSED(num_numbers),
                                    std::vector<std::string> const &EP_UNUSED(alphas),
                                    std::vector<bool> const &EP_UNUSED(alphas_blank),
                                    std::vector<Real64> const &EP_UNUSED(numbers),
                                    std::vector<bool> const &EP_UNUSED(numbers_blank))
{
    // using namespace InputProcessor;

    // bool has_error = OverallErrorFlag;

    // EXPECT_FALSE( OverallErrorFlag );

    // auto index = FindItemInSortedList( name, ListOfObjects, NumObjectDefs );

    // EXPECT_GT( index, 0 ) << "Could not find \"" << name << "\". Make sure to run process_idf first.";
    // if ( index < 1 ) return false;

    // index = iListOfObjects( index );
    // index = ObjectStartRecord( index );

    // EXPECT_EQ( name, IDFRecords( index ).Name );
    // if ( name != IDFRecords( index ).Name ) has_error = true;
    // EXPECT_EQ( num_alphas, IDFRecords( index ).NumAlphas );
    // if ( num_alphas != IDFRecords( index ).NumAlphas ) has_error = true;
    // EXPECT_EQ( num_numbers, IDFRecords( index ).NumNumbers );
    // if ( num_numbers != IDFRecords( index ).NumNumbers ) has_error = true;
    // if ( ! compare_containers( alphas, IDFRecords( index ).Alphas ) ) has_error = true;
    // if ( ! compare_containers( alphas_blank, IDFRecords( index ).AlphBlank ) ) has_error = true;
    // if ( ! compare_containers( numbers, IDFRecords( index ).Numbers ) ) has_error = true;
    // if ( ! compare_containers( numbers_blank, IDFRecords( index ).NumBlank ) ) has_error = true;

    // return ! has_error;
    return false;
}

} // namespace EnergyPlus
