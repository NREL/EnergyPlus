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

#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/OutputFiles.hh>

#include "DataStringGlobals.hh"
#include <ObjexxFCL/gio.hh>
#include <fmt/format.h>
#include <fmt/ostream.h>

namespace EnergyPlus {

std::ostream &open_out_stream(int const fileID, const std::string &fileName)
{
    IOFlags flags;
    flags.ACTION("write");
    flags.STATUS("UNKNOWN");
    flags.POSITION("APPEND");
    ObjexxFCL::gio::open(fileID, fileName, flags);
    return *ObjexxFCL::gio::out_stream(fileID);
}

std::ostream &get_out_stream(int const FileID, const std::string &fileName)
{
    if (!ObjexxFCL::gio::out_stream(FileID)) {
        return open_out_stream(FileID, fileName);
    } else {
        return *ObjexxFCL::gio::out_stream(FileID);
    }
}

bool OutputFiles::OutputFile::good() const
{
    if (os) {
        return os->good();
    } else {
        return false;
    }
}

void OutputFiles::OutputFile::close()
{
    os.reset();
}

void OutputFiles::OutputFile::open_as_stringstream()
{
    os = std::unique_ptr<std::iostream>(new std::stringstream());
}

std::string OutputFiles::OutputFile::get_output()
{
    auto *ss = dynamic_cast<std::stringstream *>(os.get());
    if (ss) {
        return ss->str();
    } else {
        return "";
    }
}

OutputFiles::OutputFile::OutputFile(std::string FileName) : fileName(std::move(FileName))
{
}

void OutputFiles::OutputFile::open()
{
    os = std::unique_ptr<std::iostream>(new std::fstream(fileName.c_str(), std::ios_base::in | std::ios_base::out | std::ios_base::trunc));
}

std::vector<std::string> OutputFiles::OutputFile::getLines()
{
    if (os) {
        os->flush();
        const auto last_pos = os->tellg();
        std::string line;
        std::vector<std::string> lines;
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

void OutputFiles::OutputFile::open_at_end()
{
    os = std::unique_ptr<std::iostream>(new std::fstream(fileName.c_str(), std::ios_base::in | std::ios_base::out | std::ios_base::ate));
}

OutputFiles::GIOOutputFile::GIOOutputFile(int const FileID, std::string FileName)
    : fileID{FileID}, fileName{std::move(FileName)}, os{get_out_stream(FileID, FileName)}
{
}

void OutputFiles::GIOOutputFile::close()
{
    ObjexxFCL::gio::close(fileID);
}

void OutputFiles::GIOOutputFile::open_at_end()
{
    os = open_out_stream(fileID, fileName);
}

OutputFiles OutputFiles::makeOutputFiles()
{
    return OutputFiles();
}

OutputFiles::OutputFiles()
{
}

OutputFiles &OutputFiles::getSingleton()
{
    static OutputFiles ofs{makeOutputFiles()};
    return ofs;
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

            // matches Fortran's 'G' format
            if (specs()->type == 'N') {

                if (should_be_fixed_output(value) && fixed_will_fit(value, specs()->width - 5)) {
                    specs()->type = 'F';

                    // account for alignment with E formatted
                    specs()->width -= 4;
                    if (value == 0.0) {
                        --specs()->precision;
                    } else if (value < 1.0 && value > -1.0) {
                        // No adjustment necessary
                    } else {
                        const auto order_of_magnitude = value == 0.0 ? 1 : static_cast<int>(std::log10(std::abs(value)));
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
                    specs()->type = 'E';

                    // 0 pad the end
                    specs()->alt = true;

                    // reduce the precision to get rounding behavior
                    --specs()->precision;

                    // multiply by 10 to get the exponent we want
                    auto str = write_to_string(value * 10, *specs());

                    // swap around the first few characters and add in the leading
                    // 0 that we need to get the same formatting behavior on the rounded
                    // value that was acquired from the reduction in precision
                    auto begin = std::next(std::begin(str), specs()->width - (specs()->precision + 8));
                    std::swap(*begin, *std::next(begin));
                    std::advance(begin, 1);
                    std::swap(*std::next(begin), *std::next(begin, 2));
                    *begin = '0';
                    return write_string(str);
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
    assert(os.good());
    fmt::memory_buffer buffer;
    try {
        // Pass custom argument formatter as a template arg to vformat_to.
        fmt::vformat_to<custom_arg_formatter>(buffer, format_str, args);
    } catch (const fmt::format_error &) {
        throw fmt::format_error(fmt::format("Error with format, '{}', passed {} args", format_str, count));
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
        throw fmt::format_error(fmt::format("Error with format, '{}', passed {} args", format_str, count));
    }
    return fmt::to_string(buffer);
}

} // namespace EnergyPlus
