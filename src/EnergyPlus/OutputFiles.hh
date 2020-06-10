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

#ifndef OutputFiles_hh_INCLUDED
#define OutputFiles_hh_INCLUDED

#include <ObjexxFCL/gio.hh>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <ostream>
#include <iterator>

namespace EnergyPlus {
class OutputFile
{
public:
    void close();
    void del();
    bool good() const;

    // opens the file if it is not currently open and returns
    // a reference back to itself
    OutputFile &ensure_open(const std::string &caller, bool output_to_file = true);

    std::string fileName;
    void open(bool output_to_file = true);
    std::fstream::pos_type position() const noexcept;
    std::vector<std::string> getLines();
    void open_as_stringstream();
    std::string get_output();

private:
    explicit OutputFile(std::string FileName);
    std::unique_ptr<std::iostream> os;
    bool print_to_dev_null = false;
    template <typename... Args> friend void print(OutputFile &of, fmt::string_view format_str, const Args &... args);
    template <class InputIterator> friend void print(InputIterator first, InputIterator last, OutputFile &of, const char * delim);
    template <class InputIterator> friend void print(InputIterator first, InputIterator last, OutputFile &outputFile);
    friend class OutputFiles;
};


class OutputFiles
{
public:

    struct OutputFileName
    {
        std::string fileName;
        OutputFile open(const std::string &caller, bool output_to_file = true) {
            OutputFile of{fileName};
            of.ensure_open(caller, output_to_file);
            return of;
        }
    };

////  Remove once all gio are converted to OutputFiles
////  Use following example at each calling location
//    auto & outputFiles = OutputFiles::getSingleton();
//    outputFiles.eso.ensure_open(outputFiles.outputControl.eso);
    int open_gio(std::string const& filename, std::string const & header, bool outputControlCheck, bool showFatalError = true);
    int open_gio(std::string const& filename, std::string const & header, bool outputControlCheck, std::string const & action, bool showFatalError = true);

    struct OutputControl
    {
        OutputControl() = default;

        void getInput();

        bool csv = true;
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

    OutputFile audit{"eplusout.audit"};
    OutputFile eio{"eplusout.eio"};
    OutputFile eso{"eplusout.eso"}; // (hourly data only)

    OutputFile zsz{""};
    std::string outputZszCsvFileName{"epluszsz.csv"};
    std::string outputZszTabFileName{"epluszsz.tab"};
    std::string outputZszTxtFileName{"epluszsz.txt"};

    OutputFile ssz{""};
    std::string outputSszCsvFileName{"eplusssz.csv"};
    std::string outputSszTabFileName{"eplusssz.tab"};
    std::string outputSszTxtFileName{"eplusssz.txt"};

    OutputFile map{""};
    std::string outputMapCsvFileName{"eplusmap.csv"};
    std::string outputMapTabFileName{"eplusmap.tab"};
    std::string outputMapTxtFileName{"eplusmap.txt"};


    OutputFile mtr{"eplusout.mtr"};
    OutputFile bnd{"eplusout.bnd"};
    OutputFile rdd{"eplusout.rdd"};
    OutputFile mdd{"eplusout.mdd"};

    OutputFile csv{"eplusout.csv"};
    OutputFile mtr_csv{"eplusmtr.csv"};

    OutputFile debug{"eplusout.dbg"};

    OutputFile dfs{"eplusout.dfs"};

    OutputFileName sln{"eplusout.sln"};
    OutputFileName dxf{"eplusout.dxf"};
    OutputFileName sci{"eplusout.sci"};
    OutputFileName wrl{"eplusout.wrl"};

    OutputFileName delightIn{"eplusout.delightin"};

    OutputFile mtd{"eplusout.mtd"};
    OutputFile edd{"eplusout.edd"};
    OutputFile shade{"eplusshading.csv"};

    OutputFileName screenCsv{"eplusscreen.csv"};

    static OutputFiles &getSingleton();
    static void setSingleton(OutputFiles *newSingleton) noexcept;

private:
    static OutputFiles *&getSingletonInternal();
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

template <typename... Args> void print(OutputFile &outputFile, fmt::string_view format_str, const Args &... args)
{
    assert(outputFile.os);
    EnergyPlus::vprint(*outputFile.os, format_str, fmt::make_format_args(args...), sizeof...(Args));
}

template <class InputIterator> void print(InputIterator first, InputIterator last, OutputFile &outputFile, const char * delim)
{
    assert(outputFile.os);
    std::copy(first, last, std::ostream_iterator<typename std::iterator_traits<InputIterator>::value_type>(*outputFile.os, delim));
}

template <class InputIterator> void print(InputIterator first, InputIterator last, OutputFile &outputFile)
{
    assert(outputFile.os);
    std::copy(first, last, std::ostream_iterator<typename std::iterator_traits<InputIterator>::value_type>(*outputFile.os));
}

template <typename... Args> std::string format(fmt::string_view format_str, const Args &... args)
{
    return EnergyPlus::vprint(format_str, fmt::make_format_args(args...), sizeof...(Args));
}

} // namespace EnergyPlus

#endif
