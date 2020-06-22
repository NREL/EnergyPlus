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

#include <fstream>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <ostream>
#include <vector>
#include <cassert>

namespace EnergyPlus {


class InputOutputFile
{
public:
    void close();
    void del();
    bool good() const;

    // opens the file if it is not currently open and returns
    // a reference back to itself
    InputOutputFile &ensure_open(const std::string &caller);

    std::string fileName;
    void open(const bool forAppend = false);
    std::fstream::pos_type position() const noexcept;
    std::vector<std::string> getLines();
    void open_as_stringstream();
    std::string get_output();
    void flush();
    explicit InputOutputFile(std::string FileName);

private:
    std::unique_ptr<std::iostream> os;
    template <typename... Args> friend void print(InputOutputFile &of, fmt::string_view format_str, const Args &... args);
    friend class OutputFiles;
};


class OutputFiles
{
public:

    struct OutputFileName
    {
        std::string fileName;
        InputOutputFile open(const std::string &caller) {
            InputOutputFile of{fileName};
            of.ensure_open(caller);
            return of;
        }
        InputOutputFile try_open() {
            InputOutputFile of{fileName};
            of.open();
            return of;
        }
    };

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

    InputOutputFile debug{"eplusout.dbg"};

    InputOutputFile dfs{"eplusout.dfs"};

    OutputFileName sln{"eplusout.sln"};
    OutputFileName dxf{"eplusout.dxf"};
    OutputFileName sci{"eplusout.sci"};
    OutputFileName wrl{"eplusout.wrl"};

    OutputFileName delightIn{"eplusout.delightin"};

    InputOutputFile mtd{"eplusout.mtd"};
    InputOutputFile edd{"eplusout.edd"};
    InputOutputFile shade{"eplusshading.csv"};

    OutputFileName screenCsv{"eplusscreen.csv"};
    OutputFileName endFile{"eplusout.end"};

    static OutputFiles &getSingleton();
    static void setSingleton(OutputFiles *newSingleton) noexcept;

private:
    static OutputFiles *&getSingletonInternal();
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
    assert(outputFile.os);
    EnergyPlus::vprint(*outputFile.os, format_str, fmt::make_format_args(args...), sizeof...(Args));
}

template <typename... Args> std::string format(fmt::string_view format_str, const Args &... args)
{
    return EnergyPlus::vprint(format_str, fmt::make_format_args(args...), sizeof...(Args));
}

} // namespace EnergyPlus

#endif
