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

#include <EnergyPlus/IOFiles.hh>

#include "Data/EnergyPlusData.hh"
#include "DataStringGlobals.hh"
#include "FileSystem.hh"
#include "InputProcessing/EmbeddedEpJSONSchema.hh"
#include "InputProcessing/InputProcessor.hh"
#include "UtilityRoutines.hh"

#include "nlohmann/json.hpp"
#include <fmt/format.h>
#include <stdexcept>

namespace EnergyPlus {

InputFile &InputFile::ensure_open(EnergyPlusData &state, const std::string &caller, bool output_to_file)
{
    if (!good()) {
        open(false, output_to_file);
    }
    if (!good()) {
        ShowFatalError(state, fmt::format("{}: Could not open file {} for input (read).", caller, filePath.string()));
    }
    return *this;
}

bool InputFile::good() const noexcept
{
    if (is) {
        return is->good();
    } else {
        return false;
    }
}

void InputFile::close()
{
    is.reset();
}

InputFile::ReadResult<std::string> InputFile::readLine() noexcept
{
    if (is) {
        std::string line;
        std::getline(*is, line);
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        return {std::move(line), is->eof(), is->good()};
    } else {
        return {"", true, false};
    }
}

InputFile::InputFile(fs::path FilePath) : filePath(std::move(FilePath))
{
}

std::ostream::pos_type InputFile::position() const noexcept
{
    return is->tellg();
}

void InputFile::open(bool, bool)
{
    is = std::unique_ptr<std::istream>(new std::fstream(filePath.c_str(), std::ios_base::in | std::ios_base::binary));
    is->imbue(std::locale("C"));
}

std::string InputFile::error_state_to_string() const
{
    const auto state = rdstate();

    if (!is_open()) {
        return "file not opened'";
    }

    if (state == std::ios_base::failbit) {
        return "io operation failed";
    } else if (state == std::ios_base::badbit) {
        return "irrecoverable stream error";
    } else if (state == std::ios_base::eofbit) {
        return "end of file reached";
    } else {
        return "no error";
    }
}

std::istream::iostate InputFile::rdstate() const noexcept
{
    if (is) {
        return is->rdstate();
    } else {
        return std::ios_base::badbit;
    }
}

bool InputFile::is_open() const noexcept
{
    if (is) {
        auto *ss = dynamic_cast<std::ifstream *>(is.get());
        if (ss) {
            return ss->is_open();
        } else {
            return true;
        }
    } else {
        return false;
    }
}

void InputFile::backspace() noexcept
{
    if (is) {
        is->clear();
        std::streamoff g1(is->tellg()); // Current position
        is->seekg(0, std::ios::beg);    // Beginning of file
        std::streampos const g0(is->tellg());
        is->seekg(g1, std::ios::beg); // Restore position
        if (g1 > g0) --g1;
        while (g1 > g0) {
            is->seekg(--g1, std::ios::beg); // Backup by 1
            if (is->peek() == '\n') {       // Found end of previous record
                is->seekg(++g1, std::ios::beg);
                break;
            }
        }
    }
}

InputOutputFile &InputOutputFile::ensure_open(EnergyPlusData &state, const std::string &caller, bool output_to_file)
{
    if (!good()) {
        open(false, output_to_file);
    }
    if (!good()) {
        ShowFatalError(state, fmt::format("{}: Could not open file {} for output (write).", caller, filePath.string()));
    }
    return *this;
}

bool InputOutputFile::good() const
{
    if (os && print_to_dev_null && os->bad()) { // badbit is set
        return true;
    } else if (os) {
        return os->good();
    } else {
        return false;
    }
}

void InputOutputFile::close()
{
    os.reset();
}

void InputOutputFile::del()
{
    if (os) {
        os.reset();
        FileSystem::removeFile(filePath);
    }
}

void InputOutputFile::open_as_stringstream()
{
    os = std::unique_ptr<std::iostream>(new std::stringstream());
}

void InputOutputFile::flush()
{
    if (os) {
        os->flush();
    }
}

std::string InputOutputFile::get_output()
{
    auto *ss = dynamic_cast<std::stringstream *>(os.get());
    if (ss) {
        return ss->str();
    } else {
        return "";
    }
}

InputOutputFile::InputOutputFile(fs::path FilePath, const bool DefaultToStdout) : filePath{std::move(FilePath)}, defaultToStdOut{DefaultToStdout}
{
}

std::ostream::pos_type InputOutputFile::position() const noexcept
{
    return os->tellg();
}

void InputOutputFile::open(const bool forAppend, bool output_to_file)
{
    auto appendMode = [=]() {
        if (forAppend) {
            return std::ios_base::app;
        } else {
            return std::ios_base::trunc;
        }
    }();
    if (!output_to_file) {
        os = std::unique_ptr<std::iostream>(new std::iostream(nullptr));
        os->imbue(std::locale("C"));
        print_to_dev_null = true;
    } else {
        os = std::unique_ptr<std::iostream>(new std::fstream(filePath.c_str(), std::ios_base::in | std::ios_base::out | appendMode));
        os->imbue(std::locale("C"));
        print_to_dev_null = false;
    }
}

std::vector<std::string> InputOutputFile::getLines()
{
    if (os) {
        // avoid saving and reloading the file by simply reading the current input stream
        os->flush();
        const auto last_pos = os->tellg();
        std::string line;
        std::vector<std::string> lines;
        os->seekg(0);

        while (std::getline(*os, line)) {
            lines.push_back(line);
        }

        // after getline is done, we're at eof/fail bit
        os->clear();
        os->seekg(last_pos);
        return lines;
    }
    return std::vector<std::string>();
}

void IOFiles::OutputControl::getInput(EnergyPlusData &state)
{
    auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find("OutputControl:Files");
    if (instances != state.dataInputProcessing->inputProcessor->epJSON.end()) {

        auto find_input = [=, &state](nlohmann::json const &fields, std::string const &field_name) -> std::string {
            std::string input;
            auto found = fields.find(field_name);
            if (found != fields.end()) {
                input = found.value().get<std::string>();
                input = UtilityRoutines::MakeUPPERCase(input);
            } else {
                state.dataInputProcessing->inputProcessor->getDefaultValue(state, "OutputControl:Files", field_name, input);
            }
            return input;
        };

        auto boolean_choice = [=, &state](std::string const &input) -> bool {
            if (input == "YES") {
                return true;
            } else if (input == "NO") {
                return false;
            }
            ShowFatalError(state, "Invalid boolean Yes/No choice input");
            return true;
        };

        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            auto const &fields = instance.value();

            { // "output_csv"
                csv = boolean_choice(find_input(fields, "output_csv"));
            }
            { // "output_mtr"
                mtr = boolean_choice(find_input(fields, "output_mtr"));
            }
            { // "output_eso"
                eso = boolean_choice(find_input(fields, "output_eso"));
            }
            { // "output_eio"
                eio = boolean_choice(find_input(fields, "output_eio"));
            }
            { // "output_audit"
                audit = boolean_choice(find_input(fields, "output_audit"));
            }
            { // "output_zone_sizing"
                zsz = boolean_choice(find_input(fields, "output_zone_sizing"));
            }
            { // "output_system_sizing"
                ssz = boolean_choice(find_input(fields, "output_system_sizing"));
            }
            { // "output_dxf"
                dxf = boolean_choice(find_input(fields, "output_dxf"));
            }
            { // "output_bnd"
                bnd = boolean_choice(find_input(fields, "output_bnd"));
            }
            { // "output_rdd"
                rdd = boolean_choice(find_input(fields, "output_rdd"));
            }
            { // "output_mdd"
                mdd = boolean_choice(find_input(fields, "output_mdd"));
            }
            { // "output_mtd"
                mtd = boolean_choice(find_input(fields, "output_mtd"));
            }
            { // "output_end"
                end = boolean_choice(find_input(fields, "output_end"));
            }
            { // "output_shd"
                shd = boolean_choice(find_input(fields, "output_shd"));
            }
            { // "output_dfs"
                dfs = boolean_choice(find_input(fields, "output_dfs"));
            }
            { // "output_glhe"
                glhe = boolean_choice(find_input(fields, "output_glhe"));
            }
            { // "output_delightin"
                delightin = boolean_choice(find_input(fields, "output_delightin"));
            }
            { // "output_delighteldmp"
                delighteldmp = boolean_choice(find_input(fields, "output_delighteldmp"));
            }
            { // "output_delightdfdmp"
                delightdfdmp = boolean_choice(find_input(fields, "output_delightdfdmp"));
            }
            { // "output_edd"
                edd = boolean_choice(find_input(fields, "output_edd"));
            }
            { // "output_dbg"
                dbg = boolean_choice(find_input(fields, "output_dbg"));
            }
            { // "output_perflog"
                perflog = boolean_choice(find_input(fields, "output_perflog"));
            }
            { // "output_sln"
                sln = boolean_choice(find_input(fields, "output_sln"));
            }
            { // "output_sci"
                sci = boolean_choice(find_input(fields, "output_sci"));
            }
            { // "output_wrl"
                wrl = boolean_choice(find_input(fields, "output_wrl"));
            }
            { // "output_screen"
                screen = boolean_choice(find_input(fields, "output_screen"));
            }
            { // "output_tarcog"
                tarcog = boolean_choice(find_input(fields, "output_tarcog"));
            }
            { // "output_extshd"
                extshd = boolean_choice(find_input(fields, "output_extshd"));
            }
            { // "json"
                json = boolean_choice(find_input(fields, "output_json"));
            }
            { // "tabular"
                tabular = boolean_choice(find_input(fields, "output_tabular"));
            }
            { // "sqlite"
                sqlite = boolean_choice(find_input(fields, "output_sqlite"));
            }
        }
    }
}

void IOFiles::flushAll()
{

    audit.flush();
    eio.flush();
    eso.flush();
    zsz.flush();
    ssz.flush();
    map.flush();
    mtr.flush();
    bnd.flush();
    rdd.flush();
    mdd.flush();
    debug.flush();
    dfs.flush();
    mtd.flush();
    edd.flush();
    shade.flush();
    csv.flush();

    if (err_stream) {
        err_stream->flush();
    }
    if (json.json_stream) {
        json.json_stream->flush();
    }
    if (json.json_TSstream_Zone) {
        json.json_TSstream_Zone->flush();
    }
    if (json.json_TSstream_HVAC) {
        json.json_TSstream_HVAC->flush();
    }
    if (json.json_TSstream) {
        json.json_TSstream->flush();
    }
    if (json.json_HRstream) {
        json.json_HRstream->flush();
    }
    if (json.json_MNstream) {
        json.json_MNstream->flush();
    }
    if (json.json_DYstream) {
        json.json_DYstream->flush();
    }
    if (json.json_SMstream) {
        json.json_SMstream->flush();
    }
    if (json.json_YRstream) {
        json.json_YRstream->flush();
    }
    if (json.cbor_stream) {
        json.cbor_stream->flush();
    }
    if (json.cbor_TSstream_Zone) {
        json.cbor_TSstream_Zone->flush();
    }
    if (json.cbor_TSstream_HVAC) {
        json.cbor_TSstream_HVAC->flush();
    }
    if (json.cbor_TSstream) {
        json.cbor_TSstream->flush();
    }
    if (json.cbor_HRstream) {
        json.cbor_HRstream->flush();
    }
    if (json.cbor_MNstream) {
        json.cbor_MNstream->flush();
    }
    if (json.cbor_DYstream) {
        json.cbor_DYstream->flush();
    }
    if (json.cbor_SMstream) {
        json.cbor_SMstream->flush();
    }
    if (json.cbor_YRstream) {
        json.cbor_YRstream->flush();
    }
    if (json.msgpack_stream) {
        json.msgpack_stream->flush();
    }
    if (json.msgpack_TSstream_Zone) {
        json.msgpack_TSstream_Zone->flush();
    }
    if (json.msgpack_TSstream_HVAC) {
        json.msgpack_TSstream_HVAC->flush();
    }
    if (json.msgpack_TSstream) {
        json.msgpack_TSstream->flush();
    }
    if (json.msgpack_HRstream) {
        json.msgpack_HRstream->flush();
    }
    if (json.msgpack_MNstream) {
        json.msgpack_MNstream->flush();
    }
    if (json.msgpack_DYstream) {
        json.msgpack_DYstream->flush();
    }
    if (json.msgpack_SMstream) {
        json.msgpack_SMstream->flush();
    }
    if (json.msgpack_YRstream) {
        json.msgpack_YRstream->flush();
    }
}

using arg_formatter = fmt::arg_formatter<fmt::buffer_range<char>>;

class custom_arg_formatter : public arg_formatter
{
public:
    explicit custom_arg_formatter(fmt::format_context &ctx, fmt::format_parse_context *parse_ctx = nullptr, fmt::format_specs *spec = nullptr)
        : arg_formatter(ctx, parse_ctx, spec)

    {
    }

    using arg_formatter::operator();

    static constexpr bool should_be_fixed_output(const Real64 value)
    {
        return (value >= 0.099999999999999995 || value <= -0.099999999999999995) || (value == 0.0) || (value == -0.0);
    }

    static bool fixed_will_fit(const Real64 value, const int places)
    {
        if (value < 1.0 && value > -1.0) {
            return true;
        } else {
            return static_cast<int>(std::log10(std::abs(value))) < places;
        }
    }

    static std::string zero_pad_exponent(std::string str)
    {
        // if necessary, pad the exponent with a 0 to match the old formatting from Objexx
        if (str.size() > 3) {
            if (!std::isdigit(str[str.size() - 3])) {
                // wants a 0 inserted
                str.insert(str.size() - 2, "0");
            }
        }
        return str;
    }

    static std::string write_to_string(const Real64 value, fmt::format_specs &specs)
    {
        std::string str;

        struct string_ref
        {
            std::reference_wrapper<std::string> ref;
            std::back_insert_iterator<std::string> begin()
            {
                return std::back_inserter(ref.get());
            }

            using value_type = std::string::value_type;
            using iterator = std::back_insert_iterator<std::string>;
        };

        fmt::internal::basic_writer<string_ref> bw(string_ref{str});
        bw.write(value, specs);
        return str;
    }

    iterator write_string(const std::string &str)
    {
        // write one character at a time to avoid any spec formatting from {fmt}
        // which may truncate our output otherwise
        std::for_each(std::begin(str), std::end(str), [&](const char c) { writer().write(c); });
        return out();
    }

    iterator operator()(Real64 const value)
    {
        if (specs()) {
            const auto next_float = [](const Real64 value) {
                if (std::signbit(value)) {
                    if (value == -0.0) {
                        return value;
                    } else {
                        return std::nextafter(value, std::numeric_limits<decltype(value)>::lowest());
                    }
                } else {
                    if (value == 0.0) {
                        return value;
                    } else {
                        return std::nextafter(value, std::numeric_limits<decltype(value)>::max());
                    }
                }
            };

            // matches Fortran's 'E' format
            if (specs()->type == 'Z') {
                // The Fortran 'G' format insists on a leading 0, even though
                // that actually means losing data
                specs()->type = 'E';

                // 0 pad the end
                specs()->alt = true;

                if (specs()->precision > 0) {
                    // reduce the precision to get rounding behavior
                    --specs()->precision;
                }

                // multiply by 10 to get the exponent we want
                auto str = write_to_string(value * 10, *specs());

                // we need "space" to insert our leading 0
                if (str.front() != ' ') {
                    str.insert(str.begin(), ' ');
                }

                auto begin = std::find(std::begin(str), std::end(str), '.');
                // ' -1.2345E15'
                //     ^
                std::swap(*begin, *std::prev(begin));
                // ' -.12345E15'
                //     ^
                std::advance(begin, -2);
                // ' -.12345E15'
                //   ^
                if (*begin != ' ') {
                    // found a sign
                    std::swap(*begin, *std::prev(begin));
                    // '- .12345E15'
                    //   ^
                }
                // '-0.12345E15'
                //   ^
                *begin = '0';
                return write_string(str);
            } else if (specs()->type == 'S') {
                // matches Fortran's 'G', but stripped of whitespace
                specs()->type = 'N';
                return write_string(stripped(write_to_string(value, *specs())));
            } else if (specs()->type == 'N') {
                // matches Fortran's 'G' format

                if (specs()->width == 0 && specs()->precision == -1) {
                    return write_string(format("{:20N}", value));
                } else if (should_be_fixed_output(value) && fixed_will_fit(value, specs()->width - 5)) {
                    specs()->type = 'F';

                    // account for alignment with E formatted
                    specs()->width -= 4;
                    if (value == 0.0) {
                        --specs()->precision;
                    } else if (value < 1.0 && value > -1.0) {
                        // No adjustment necessary
                    } else if (specs()->precision == -1) {
                        const auto order_of_magnitude = static_cast<int>(std::log10(std::abs(value)));
                        specs()->precision = specs()->width - (order_of_magnitude + 2);
                    } else {
                        const auto order_of_magnitude = static_cast<int>(std::log10(std::abs(value)));
                        specs()->precision -= (order_of_magnitude + 1);
                    }

                    // if precision adjustment would result in negative, make it 0 to get rounding
                    // and adjust spacing
                    if (specs()->precision <= 0) {
                        specs()->width -= 1;
                        specs()->precision = 0;
                    }

                    (*this)(value);

                    // When precision hit 0, add . to match Fortran formatting
                    if (specs()->precision == 0) {
                        write_string(".");
                    }

                    // write the last 4 chars
                    return write_string("    ");
                } else {
                    // The Fortran 'G' format insists on a leading 0, even though
                    // that actually means losing data
                    specs()->type = 'Z';
                    return (*this)(value);
                }
            } else if (specs()->type == 'R') { // matches RoundSigDigits() behavior
                // push the value up a tad to get the same rounding behavior as Objexx
                const auto fixed_output = should_be_fixed_output(value);

                if (fixed_output) {
                    specs()->type = 'F';

                    if (value > 100000.0) {
                        const auto digits10 = static_cast<int>(std::log10(value));
                        // we cannot represent this value to the require precision, truncate the floating
                        // point portion
                        if (digits10 + specs()->precision >= std::numeric_limits<decltype(value)>::max_digits10) {
                            specs()->precision = 0;
                            // add '.' to match old RoundSigDigits
                            const auto str = write_to_string(value, *specs()) + '.';
                            return write_string(str);
                        } else {
                            return (*this)(value);
                        }
                    } else {
                        if (value == 0.0 || value == -0.0) {
                            return (*this)(0.0);
                        } else {
                            // nudge up to next rounded value
                            return (*this)(next_float(next_float(next_float(value))));
                        }
                    }
                } else {
                    specs()->type = 'E';
                    return write_string(zero_pad_exponent(write_to_string(next_float(value), *specs())));
                }
            } else if (specs()->type == 'T') { // matches TrimSigDigits behavior
                const auto fixed_output = should_be_fixed_output(value);

                if (fixed_output) {
                    const auto magnitude = std::pow(10, specs()->precision);
                    const auto adjusted = (value * magnitude) + 0.0001;
                    const auto truncated = std::trunc(adjusted) / magnitude;
                    specs()->type = 'F';
                    return (*this)(truncated);
                } else {
                    specs()->type = 'E';
                    specs()->precision += 2;

                    // write the `E` formatted float to a std::string
                    auto str = zero_pad_exponent(write_to_string(value, *specs()));

                    // Erase last 2 numbers to truncate the value
                    const auto E_itr = std::find(begin(str), end(str), 'E');
                    if (E_itr != str.end()) {
                        str.erase(std::prev(E_itr, 2), E_itr);
                    }

                    return write_string(str);
                }
            }
        }
        return arg_formatter::operator()(value);
    }
}; // namespace EnergyPlus

void vprint(std::ostream &os, fmt::string_view format_str, fmt::format_args args, const std::size_t count)
{
    //    assert(os.good());
    fmt::memory_buffer buffer;
    try {
        // Pass custom argument formatter as a template arg to vformat_to.
        fmt::vformat_to<custom_arg_formatter>(buffer, format_str, args);
    } catch (const fmt::format_error &) {
        throw FatalError(fmt::format("Error with format, '{}', passed {} args", format_str, count));
    }
    os.write(buffer.data(), buffer.size());
}

std::string vprint(fmt::string_view format_str, fmt::format_args args, const std::size_t count)
{
    fmt::memory_buffer buffer;
    try {
        // Pass custom argument formatter as a template arg to vformat_to.
        fmt::vformat_to<custom_arg_formatter>(buffer, format_str, args);
    } catch (const fmt::format_error &) {
        throw FatalError(fmt::format("Error with format, '{}', passed {} args", format_str, count));
    }
    return fmt::to_string(buffer);
}

} // namespace EnergyPlus
