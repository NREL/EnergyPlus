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

#ifndef FileSystem_hh_INCLUDED
#define FileSystem_hh_INCLUDED

#include <algorithm>
#include <string>

#if __has_include(<filesystem>)
    #include <filesystem>
    namespace fs = std::filesystem;
#elif __has_include(<experimental/filesystem>)
    #include <experimental/filesystem>
    namespace fs = std::experimental::filesystem;
#else
    #error "no filesystem support"
#endif

namespace EnergyPlus {

namespace FileSystem {

    extern std::string const exeExtension;

    // Similar to fs::path::make_preferred, but also does '\\' => '/' conversion on POSIX, which make_preferred does not do
    void makeNativePath(fs::path &path);

    [[no_discard]] fs::path getFileName(fs::path const &filePath);

    // Returns the parent directory of a path. This implementation differs from filesystem::path::parent_path because it treats trailing separators
    // differently.
    // | s      | getParentDirectoryPath(s) | fs::path(s).parent_path() |
    // |--------|---------------------------|---------------------------|
    // | a/b/c  | "a/b"                     | "a/b"                     |
    // | a/b/c/ | "a/b"                     | "a/b/c"                   |
    // | a.idf  | "./"                      | ""                        |
    [[no_discard]] fs::path getParentDirectoryPath(fs::path const &filePath);

    [[no_discard]] fs::path getAbsolutePath(fs::path const &filePath);

    [[no_discard]] fs::path getProgramPath();

    // For `a/b/c.txt.idf` it returns `idf`, i.e. anything after last dot, **not including the dot** (unlike fs::path::extension() which includes it)
    [[no_discard]] fs::path getFileExtension(fs::path const &fileName);

    // Turns a/b/c.txt.idf into a/b/c.txt
    [[no_discard]] fs::path removeFileExtension(fs::path const &fileName);

    // Creates a directory if it doesn't already exists
    void makeDirectory(fs::path const &directoryPath);

    bool pathExists(fs::path const &path);

    bool directoryExists(fs::path const &directoryPath);

    bool fileExists(fs::path const &filePath);

    // Checks that fileExists(filePath), if so tries to rename to destination, falling back on copy+remove if failed (if trying to do move accross
    // devices for eg)
    void moveFile(fs::path const &filePath, fs::path const &destination);

    int systemCall(fs::path const &command);

    // Returns false if not fileExists(filePath), or if filePath cannot be removed
    bool removeFile(fs::path const &filePath);

    // On Windows, this just copies the file. On Unix, it creates a symlink
    // Starts by checking that fileExists(filePath) is true
    void linkFile(fs::path const &fileName, fs::path const &link);

} // namespace FileSystem
} // namespace EnergyPlus
#endif
