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

#ifndef IOFiles_hh_INCLUDED
#define IOFiles_hh_INCLUDED

#include <fstream>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <iostream>
#include <ostream>
#include <vector>
#include <cassert>

namespace EnergyPlus {

class InputFile
{
public:
    template<typename Type>
    struct ReadResult {
        ReadResult(Type data_, bool eof_, bool good_) : data{std::move(data_)}, eof{eof_}, good{good_} {}

        // Update the current eof/good state from the incoming value
        // but only update the `data` member if the state is good
        // The idea is to keep consistency with the operator>> that was used
        // from gio
        void update(ReadResult &&other) {
            eof = other.eof;
            good = other.good;
            if (good) {
                data = std::move(other.data);
            }
        }

        Type data;
        bool eof;
        bool good;
    };

    void close();
    bool good() const noexcept;

    bool is_open() const noexcept;

    void backspace () noexcept;

    std::string error_state_to_string() const;

    // opens the file if it is not currently open and returns
    // a reference back to itself
    InputFile &ensure_open(const std::string &caller, bool output_to_file = true);
    std::istream::iostate rdstate() const noexcept;

    std::string fileName;
    void open(bool = false, bool = true);
    std::fstream::pos_type position() const noexcept;

    void rewind() noexcept { if (is) { is->seekg(0); } }

    ReadResult<std::string> readLine() noexcept;

    template<typename T>
    ReadResult<T> read() noexcept {
        if (is) {
            T result;
            *is >> result;
            return ReadResult<T>{result, is->eof(), is->good()};
        } else {
            return ReadResult<T>{T{}, true, false};
        }
    }

    explicit InputFile(std::string FileName);

private:
    std::unique_ptr<std::istream> is;
    friend class IOFiles;
};

class InputOutputFile
{
public:
    std::string fileName;
    bool defaultToStdOut = false;

    void close();
    void del();
    bool good() const;

    // opens the file if it is not currently open and returns
    // a reference back to itself
    InputOutputFile &ensure_open(const std::string &caller, bool output_to_file = true);

    void open(const bool forAppend = false, bool output_to_file = true);
    std::fstream::pos_type position() const noexcept;
    std::vector<std::string> getLines();
    void open_as_stringstream();
    std::string get_output();
    void flush();
    explicit InputOutputFile(std::string FileName, const bool DefaultToStdOut = false);

private:
    std::unique_ptr<std::iostream> os;
    bool print_to_dev_null = false;
    template <typename... Args> friend void print(InputOutputFile &of, fmt::string_view format_str, const Args &... args);
    template <class InputIterator> friend void print(InputIterator first, InputIterator last, InputOutputFile &outputFile, const char * delim);
    template <class InputIterator> friend void print(InputIterator first, InputIterator last, InputOutputFile &outputFile);
    friend class IOFiles;
};

template <typename FileType> struct IOFileName
{
    std::string fileName;
    FileType open(const std::string &caller, bool output_to_file = true)
    {
        FileType file{fileName};
        file.ensure_open(caller, output_to_file);
        return file;
    }
    FileType try_open(bool output_to_file = true)
    {
        FileType file{fileName};
        file.open(false, output_to_file);
        return file;
    }
};

using InputOutputFileName = IOFileName<InputOutputFile>;
using InputFileName = IOFileName<InputFile>;

struct JsonOutputStreams
{
    std::unique_ptr<std::ostream> json_stream; // Internal stream used for json output
    std::unique_ptr<std::ostream> json_TSstream_Zone;
    std::unique_ptr<std::ostream> json_TSstream_HVAC;
    std::unique_ptr<std::ostream> json_TSstream;
    std::unique_ptr<std::ostream> json_HRstream;
    std::unique_ptr<std::ostream> json_MNstream;
    std::unique_ptr<std::ostream> json_DYstream;
    std::unique_ptr<std::ostream> json_SMstream;
    std::unique_ptr<std::ostream> json_YRstream;
    std::unique_ptr<std::ostream> cbor_stream; // Internal stream used for cbor output
    std::unique_ptr<std::ostream> cbor_TSstream_Zone;
    std::unique_ptr<std::ostream> cbor_TSstream_HVAC;
    std::unique_ptr<std::ostream> cbor_TSstream;
    std::unique_ptr<std::ostream> cbor_HRstream;
    std::unique_ptr<std::ostream> cbor_MNstream;
    std::unique_ptr<std::ostream> cbor_DYstream;
    std::unique_ptr<std::ostream> cbor_SMstream;
    std::unique_ptr<std::ostream> cbor_YRstream;
    std::unique_ptr<std::ostream> msgpack_stream; // Internal stream used for messagepack output
    std::unique_ptr<std::ostream> msgpack_TSstream_Zone;
    std::unique_ptr<std::ostream> msgpack_TSstream_HVAC;
    std::unique_ptr<std::ostream> msgpack_TSstream;
    std::unique_ptr<std::ostream> msgpack_HRstream;
    std::unique_ptr<std::ostream> msgpack_MNstream;
    std::unique_ptr<std::ostream> msgpack_DYstream;
    std::unique_ptr<std::ostream> msgpack_SMstream;
    std::unique_ptr<std::ostream> msgpack_YRstream;

    std::string outputJsonFileName;
    std::string outputTSHvacJsonFileName;
    std::string outputTSZoneJsonFileName;
    std::string outputTSJsonFileName;
    std::string outputYRJsonFileName;
    std::string outputMNJsonFileName;
    std::string outputDYJsonFileName;
    std::string outputHRJsonFileName;
    std::string outputSMJsonFileName;
    std::string outputCborFileName;
    std::string outputTSHvacCborFileName;
    std::string outputTSZoneCborFileName;
    std::string outputTSCborFileName;
    std::string outputYRCborFileName;
    std::string outputMNCborFileName;
    std::string outputDYCborFileName;
    std::string outputHRCborFileName;
    std::string outputSMCborFileName;
    std::string outputMsgPackFileName;
    std::string outputTSHvacMsgPackFileName;
    std::string outputTSZoneMsgPackFileName;
    std::string outputTSMsgPackFileName;
    std::string outputYRMsgPackFileName;
    std::string outputMNMsgPackFileName;
    std::string outputDYMsgPackFileName;
    std::string outputHRMsgPackFileName;
    std::string outputSMMsgPackFileName;
};

class IOFiles
{
public:

    struct OutputControl
    {
        OutputControl() = default;

        void getInput();

        bool csv = false;
        bool mtr = true;
        bool eso = true;
        bool eio = true;
        bool audit = true;
        bool zsz = true;
        bool ssz = true;
        bool dxf = true;
        bool bnd = true;
        bool rdd = true;
        bool mdd = true;
        bool mtd = true;
        bool end = true;
        bool shd = true;
        bool dfs = true;
        bool glhe = true;
        bool delightin = true;
        bool delighteldmp = true;
        bool delightdfdmp = true;
        bool edd = true;
        bool dbg = true;
        bool perflog = true;
        bool sln = true;
        bool sci = true;
        bool wrl = true;
        bool screen = true;
        bool tarcog = true;
        bool extshd = true;
        bool json = true;
        bool tabular = true;
        bool sqlite = true;
    };

    OutputControl outputControl;

    InputOutputFile audit{"eplusout.audit"};
    InputOutputFile eio{"eplusout.eio"};
    InputOutputFile eso{"eplusout.eso"}; // (hourly data only)

    InputOutputFile zsz{""};
    std::string outputZszCsvFileName{"epluszsz.csv"};
    std::string outputZszTabFileName{"epluszsz.tab"};
    std::string outputZszTxtFileName{"epluszsz.txt"};

    InputOutputFile ssz{""};
    std::string outputSszCsvFileName{"eplusssz.csv"};
    std::string outputSszTabFileName{"eplusssz.tab"};
    std::string outputSszTxtFileName{"eplusssz.txt"};

    InputOutputFile map{""};
    std::string outputMapCsvFileName{"eplusmap.csv"};
    std::string outputMapTabFileName{"eplusmap.tab"};
    std::string outputMapTxtFileName{"eplusmap.txt"};

    InputOutputFile mtr{"eplusout.mtr"};
    InputOutputFile bnd{"eplusout.bnd"};
    InputOutputFile rdd{"eplusout.rdd"};
    InputOutputFile mdd{"eplusout.mdd"};

    InputOutputFile debug{"eplusout.dbg"};

    InputOutputFile dfs{"eplusout.dfs"};

    InputOutputFileName sln{"eplusout.sln"};
    InputOutputFileName dxf{"eplusout.dxf"};
    InputOutputFileName sci{"eplusout.sci"};
    InputOutputFileName wrl{"eplusout.wrl"};

    InputOutputFileName delightIn{"eplusout.delightin"};

    InputOutputFile mtd{"eplusout.mtd"};
    InputOutputFile edd{"eplusout.edd", true}; // write to stdout if no file never opened
    InputOutputFile shade{"eplusshading.csv"};

    InputOutputFile csv{"eplusout.csv"};
    InputOutputFile mtr_csv{"eplusmtr.csv"};

    InputOutputFileName screenCsv{"eplusscreen.csv"};
    InputOutputFileName endFile{"eplusout.end"};

    InputFileName iniFile{"EnergyPlus.ini"};

    InputFileName outputDelightEldmpFileName{"eplusout.delighteldmp"};
    InputFileName outputDelightDfdmpFileName{"eplusout.delightdfdmp"};

    // for transient uses of weather files
    // also, keeper of the currently set input weather file name
    InputFileName inputWeatherFileName{""};

    // for the persistent weather simulation, using the EPW
    // uses the file name set in `inputWeatherFileName`
    InputFile inputWeatherFile{""};

    InputFileName TempFullFileName{""};
    InputFileName inStatFileName{""};

    std::string outputErrFileName{"eplusout.err"};
    std::unique_ptr<std::ostream> err_stream;

    static IOFiles &getSingleton();
    static void setSingleton(IOFiles *newSingleton) noexcept;

    static bool hasSingleton() { return getSingletonInternal() != nullptr; }

    JsonOutputStreams json; // Internal streams used for json outputs

private:
    static IOFiles *&getSingletonInternal();
};


class SharedFileHandle
{
    std::shared_ptr<InputOutputFile> file;
    InputOutputFile *ptr()
    {
        if (!file) {
            file = std::make_shared<InputOutputFile>("");
        }

        return file.get();
    }

public:
    InputOutputFile &operator*()
    {
        return *ptr();
    }

    InputOutputFile *operator->()
    {
        return ptr();
    }
};

void vprint(std::ostream &os, fmt::string_view format_str, fmt::format_args args, const std::size_t count);
std::string vprint(fmt::string_view format_str, fmt::format_args args, const std::size_t count);

// Uses lib {fmt} (which has been accepted for C++20)
// Formatting syntax guide is here: https://fmt.dev/latest/syntax.html
// The syntax is similar to printf, but uses {} to indicate parameters to be formatted
// you must escape any {} that you want with {}, like `{{}}`
//
// Defines a custom formatting type 'R' (round_ which chooses between `E` and `G` depending
// on the value being printed.
// This is necessary for parity with the old "RoundSigDigits" utility function
//
// Defines a custom formatting type 'S' that behaves like Fortran's G type, but stripped of whitespace
// 'S' was chosen for "Stripped". It is implemented in terms of 'N'
//
// Defines a custom formatting type 'N' that behaves like Fortran's G type.
// 'N' was chosen for "Number"
//
// Defines a custom formatting type 'Z' that behaves like Fortran's E type.
// 'Z' was chosen because Fortran's 'E' format always starts with a Zero
//
// Defines a custom formatting type 'T' that that truncates the value
// to match the behavior of TrimSigDigits utility function
//
template <typename... Args> void print(std::ostream &os, fmt::string_view format_str, const Args &... args)
{
    EnergyPlus::vprint(os, format_str, fmt::make_format_args(args...), sizeof...(Args));
}

template <typename... Args> void print(InputOutputFile &outputFile, fmt::string_view format_str, const Args &... args)
{
    auto *outputStream = [&]() -> std::ostream * {
        if (outputFile.os) {
            return outputFile.os.get();
        } else {
            if (outputFile.defaultToStdOut) {
                return &std::cout;
            } else {
                assert(outputFile.os);
                return nullptr;
            }
        }
    }();

    EnergyPlus::vprint(*outputStream, format_str, fmt::make_format_args(args...), sizeof...(Args));
}

template <class InputIterator> void print(InputIterator first, InputIterator last, InputOutputFile &outputFile, const char * delim)
{
    auto *outputStream = [&]() -> std::ostream * {
        if (outputFile.os) {
            return outputFile.os.get();
        } else {
            if (outputFile.defaultToStdOut) {
                return &std::cout;
            } else {
                assert(outputFile.os);
                return nullptr;
            }
        }
    }();
    std::copy(first, last, std::ostream_iterator<typename std::iterator_traits<InputIterator>::value_type>(*outputStream, delim));
}

template <class InputIterator> void print(InputIterator first, InputIterator last, InputOutputFile &outputFile)
{
    auto *outputStream = [&]() -> std::ostream * {
        if (outputFile.os) {
            return outputFile.os.get();
        } else {
            if (outputFile.defaultToStdOut) {
                return &std::cout;
            } else {
                assert(outputFile.os);
                return nullptr;
            }
        }
    }();
    std::copy(first, last, std::ostream_iterator<typename std::iterator_traits<InputIterator>::value_type>(*outputStream));
}

template <typename... Args> std::string format(fmt::string_view format_str, const Args &... args)
{
    return EnergyPlus::vprint(format_str, fmt::make_format_args(args...), sizeof...(Args));
}

} // namespace EnergyPlus

#endif
