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

// Standard C++ library
#include <errno.h>
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef _WIN32
#include <Shlwapi.h>
#include <windows.h>
#else
#include <unistd.h>
#endif

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

// EnergyPlus Headers
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/FileSystem.hh>

namespace EnergyPlus {

namespace FileSystem {

#ifdef _WIN32
    std::string const exeExtension(".exe");
#else
    std::string const exeExtension;
#endif

    void makeNativePath(std::string &path)
    {
        // fs::path(path).make_preferred().string();
        std::replace(path.begin(), path.end(), DataStringGlobals::altpathChar, DataStringGlobals::pathChar);
    }

    std::string getFileName(std::string const &filePath)
    {
        return fs::path(filePath).filename().string();
    }

    std::string getParentDirectoryPath(std::string const &path)
    {
        std::string tempPath = path;
        if (path.at(path.size() - 1) == DataStringGlobals::pathChar) tempPath = path.substr(0, path.size() - 1);

        fs::path p(tempPath);
        tempPath = p.parent_path().string();


        // Is that below really needed? Perhaps the best way would be just to use actual fs::path (instead of std::string) and the operator/ ...
        if (tempPath.empty()) {
            tempPath = ".";
        }
        tempPath += '/';
        return tempPath;

        //int pathCharPosition = tempPath.find_last_of(DataStringGlobals::pathChar);
        //tempPath = tempPath.substr(0, pathCharPosition + 1);

        //// If empty, then current dir, but with trailing separator too: eg `./`
        //if (tempPath == "") tempPath = {'.',DataStringGlobals:: pathChar};

        //return tempPath;
    }

    std::string getAbsolutePath(std::string const &path)
    {
        /*
         * Returns the absolute path for a given relative path.
         *
         * If the relative path points to a symlink, the symlink will
         * be resolved, and this function will return the absolute path
         * of the link.
         */

        fs::path p(path);
        while (fs::is_symlink(p)) {
            p = fs::read_symlink(p);
        }

        // Not available in experimental/filesystem
        // return fs::weakly_canonical(fs::absolute(p)).string();

        p = fs::absolute(p);

        fs::path result;
        // `p` now is absolute, but it isn't necessarilly canonical.
        // If you have <filesystem>, you can use `fs::weakly_canonical`
        // This block resolves a canonical path, even if it doesn't exist (yet?) on disk.
        for (fs::path::iterator it = p.begin(); it != p.end(); ++it) {
            if (*it == fs::path("..")) {
                if (fs::is_symlink(result) || (result.filename() == fs::path(".."))) {
                    result /= *it;
                } else {
                    result = result.parent_path();
                }
            } else if (*it != fs::path(".")) {
                result /= *it;
            }
        }

        // Workaround to maintain std::string & backward compat
        std::string s = result.string();
        if (fs::is_directory(s)) {
            s += '/';
        }
        return s;
    }

    std::string getProgramPath()
    {
        /*
         * Returns the relative path to the executable file (including symlinks).
         *
         * To resolve symlinks, wrap this call in getAbsolutePath().
         */
        char executableRelativePath[1024];

#ifdef __APPLE__
        uint32_t pathSize = sizeof(executableRelativePath);
        _NSGetExecutablePath(executableRelativePath, &pathSize);
#elif __linux__
        ssize_t len = readlink("/proc/self/exe", executableRelativePath, sizeof(executableRelativePath) - 1);
        if (len == -1) {
            std::cout << "ERROR: Unable to locate executable." << std::endl;
            std::exit(EXIT_FAILURE);
        } else {
            executableRelativePath[len] = '\0';
        }
#elif _WIN32
        GetModuleFileName(NULL, executableRelativePath, sizeof(executableRelativePath));
#endif

        return std::string(executableRelativePath);
    }

    std::string getFileExtension(std::string const &fileName) {
        std::string pext = fs::path(fileName).extension().string();
        if (!pext.empty()) {
            // remove '.'
            pext = std::string(++pext.begin(), pext.end());
        }
        return pext;
    }

    std::string removeFileExtension(std::string const &fileName)
    {
        // return fs::path(fileName).stem().string();
        return fs::path(fileName).replace_extension().string();
    }

    void makeDirectory(std::string const &directoryPath)
    {
        // Create a directory if doesn't already exist
        if (pathExists(directoryPath)) { // path already exists
            if (!directoryExists(directoryPath)) {
                std::cout << "ERROR: " + getAbsolutePath(directoryPath) + " already exists and is not a directory." << std::endl;
                std::exit(EXIT_FAILURE);
            }
        } else { // directory does not already exist
            // Create_directories is recursive, create_directory isn't. I don't see why we wouldn't want recursive
            fs::create_directories(fs::path(directoryPath));
        }
    }

    bool pathExists(std::string const &path)
    {
        return fs::exists(fs::path(path));
    }

    bool directoryExists(std::string const &directoryPath)
    {
        fs::path p(directoryPath);
        return fs::exists(p) && fs::is_directory(p);
    }

    bool fileExists(std::string const &filePath)
    {
        fs::path p(filePath);
        return fs::exists(p) && !fs::is_directory(p);
    }

    void moveFile(std::string const &filePath, std::string const &destination)
    {
        if (!fs::exists(filePath)) {
            return;
        }

        try {
            fs::rename(fs::path(filePath), destination);
        } catch (fs::filesystem_error&) {
            fs::copy(filePath, destination, fs::copy_options::update_existing);
            fs::remove(filePath);
        }
    }

    int systemCall(std::string const &command)
    {
#ifdef _WIN32
        // Wrap in double quotes and pass that to system
        // Note: on Windows, system(command) will already send the command through "cmd /C command"
        // cf C:\Program Files (x86)\Windows Kits\10\Source\10.0.17763.0\ucrt\exec
        // Ends up calling something that looks like the following:
        // cmd /C ""C:\path\to\ReadVarsESO.exe" "A folder with spaces\1ZoneUncontrolled.mvi" unlimited"
        return system(("\"" + command + "\"").c_str());
#else
        return system(command.c_str());
#endif
    }

    void removeFile(std::string const &fileName)
    {
        fs::path p(fileName);
        if (!fs::exists(p)) {
            return;
        }

        fs::remove(p);
    }

    void linkFile(std::string const &fileName, std::string const &link)
    {
#ifdef _WIN32
        fs::copy(filePath, destination, fs::copy_options::update_existing);
#else
        return fs::create_symlink(fileName, link);
#endif
    }

} // namespace FileSystem
} // namespace EnergyPlus
