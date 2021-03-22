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

#ifndef DataStringGlobals_hh_INCLUDED
#define DataStringGlobals_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

struct DataStringGlobalsData : BaseGlobalStruct {

    std::string outputErrFileName;
    std::string outputMddFileName = "eplusout.mdd";
    std::string outputRddFileName = "eplusout.rdd";
    std::string outputShdFileName = "eplusout.shd";
    std::string outputTblCsvFileName = "eplustbl.csv";
    std::string outputTblHtmFileName = "eplustbl.htm";
    std::string outputTblTabFileName = "eplustbl.tab";
    std::string outputTblTxtFileName = "eplustbl.txt";
    std::string outputTblXmlFileName = "eplustbl.xml";
    std::string outputAdsFileName = "eplusADS.out";
    std::string outputGLHEFileName = "eplusout.glhe";
    std::string outputDelightOutFileName = "eplusout.delightout";
    std::string outputIperrFileName = "eplusout.iperr";
    std::string outputPerfLogFileName = "eplusout_perflog.csv";
    std::string outputSqlFileName = "eplusout.sql";
    std::string outputSqliteErrFileName = "eplussqlite.err";
    std::string outputCsvFileName = "eplusout.csv";
    std::string outputMtrCsvFileName = "eplusmtr.csv";
    std::string outputRvauditFileName = "eplusout.rvaudit";

    std::string eplusADSFileName;
    std::string idfFileNameOnly;
    std::string idfDirPathName;
    std::string outDirPathName;
    std::string inputFileNameOnly;
    std::string inputDirPathName;
    std::string outputDirPathName;
    std::string exeDirectory;
    std::string inputFileName;
    std::string inputIddFileName;
    std::string inputEpJSONSchemaFileName;
    std::string FullName;               // Full name of file to open, including path
    std::string weatherFileNameOnly;
    std::string ProgramPath;          // Path for Program from INI file
    std::string CurrentWorkingFolder; // Current working directory for run
    std::string CurrentDateTime;      // For printing current date and time at start of run
    std::string IDDVerString;         // Version information from the IDD (line 1)

    std::string VerString = "EnergyPlus, Version ${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}";
    // String that represents version information
    std::string MatchVersion = "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}";
    // String to be matched by Version object
    std::string PythonAPIVersion = "${PYTHON_API_VERSION_MAJOR}.${PYTHON_API_VERSION_MINOR}";
    // API version string to be matched when using the Python API
    
    std::string const UpperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝ";
    std::string const LowerCase = "abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüý";
    std::string const AccentedUpperCase = "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝ";
    std::string const AccentedLowerCase = "àáâãäåæçèéêëìíîïðñòóôõöøùúûüý";
    std::string const AllCase = "àáâãäåæçèéêëìíîïðñòóôõöøùúûüýÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    char const CharComma = ',';             // comma
    char const CharSemicolon = ';';         // semicolon
    char const CharTab = '\t';              // tab
    char const CharSpace = ' ';             // space

    #ifdef _WIN32
    std::string const NL = "\r\n"; // Platform newline
    #else
    std::string const NL = "\n"; // Platform newline
    #endif

    #ifdef _WIN32
    char const pathChar = '\\';
    char const altpathChar = '/';
    #elif __linux__
    char const pathChar = '/';
    char const altpathChar = '\\';
    #elif __unix__
    char const pathChar = '/';
    char const altpathChar = '\\';
    #elif __posix__
    char const pathChar = '/';
    char const altpathChar = '\\';
    #elif __APPLE__
    char const pathChar('/');
    char const altpathChar('\\');
    #else
    #error "Invalid platform detection in DataStringGlobals."
    #endif

    void clear_state() override
    {
        this->outputErrFileName.clear();
        this->outputMddFileName = "eplusout.mdd";
        this->outputRddFileName = "eplusout.rdd";
        this->outputShdFileName = "eplusout.shd";
        this->outputTblCsvFileName = "eplustbl.csv";
        this->outputTblHtmFileName = "eplustbl.htm";
        this->outputTblTabFileName = "eplustbl.tab";
        this->outputTblTxtFileName = "eplustbl.txt";
        this->outputTblXmlFileName = "eplustbl.xml";
        this->outputAdsFileName = "eplusADS.out";
        this->outputGLHEFileName = "eplusout.glhe";
        this->outputDelightOutFileName = "eplusout.delightout";
        this->outputIperrFileName = "eplusout.iperr";
        this->outputPerfLogFileName = "eplusout_perflog.csv";
        this->outputSqlFileName = "eplusout.sql";
        this->outputSqliteErrFileName = "eplussqlite.err";
        this->outputCsvFileName = "eplusout.csv";
        this->outputMtrCsvFileName = "eplusmtr.csv";
        this->outputRvauditFileName = "eplusout.rvaudit";
        this->eplusADSFileName.clear();
        this->idfFileNameOnly.clear();
        this->idfDirPathName.clear();
        this->outDirPathName.clear();
        this->inputFileNameOnly.clear();
        this->inputDirPathName.clear();
        this->outputDirPathName.clear();
        this->exeDirectory.clear();
        this->inputFileName.clear();
        this->inputIddFileName.clear();
        this->inputEpJSONSchemaFileName.clear();
        this->FullName.clear();
        this->weatherFileNameOnly.clear();
        this->ProgramPath.clear();
        this->CurrentWorkingFolder.clear();
        this->CurrentDateTime.clear();
        this->IDDVerString.clear();
        this->VerString = "EnergyPlus, Version ${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}";
        this->MatchVersion = "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}";
        this->PythonAPIVersion = "${PYTHON_API_VERSION_MAJOR}.${PYTHON_API_VERSION_MINOR}";
        
    }
};

} // namespace EnergyPlus

#endif
