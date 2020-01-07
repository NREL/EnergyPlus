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

// Standard C++ library
#include <errno.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
        std::replace(path.begin(), path.end(), DataStringGlobals::altpathChar, DataStringGlobals::pathChar);
    }

    std::string getFileName(std::string const &filePath)
    {
        int pathCharPosition = filePath.find_last_of(DataStringGlobals::pathChar);
        return filePath.substr(pathCharPosition + 1, filePath.size() - 1);
    }

    std::string getParentDirectoryPath(std::string const &path)
    {
        std::string tempPath = path;
        if (path.at(path.size() - 1) == DataStringGlobals::pathChar) tempPath = path.substr(0, path.size() - 1);

        int pathCharPosition = tempPath.find_last_of(DataStringGlobals::pathChar);
        tempPath = tempPath.substr(0, pathCharPosition + 1);

        // If empty, then current dir, but with trailing separator too: eg `./`
        if (tempPath == "") tempPath = {'.',DataStringGlobals:: pathChar};

        return tempPath;
    }

    std::string getAbsolutePath(std::string const &path)
    {

#ifdef _WIN32
        char absolutePath[1024];
        GetFullPathName(path.c_str(), sizeof(absolutePath), absolutePath, NULL);
        return std::string(absolutePath);
#else
        // If the path doesn't exist, find which of it's parents' paths does exist
        std::string parentPath = path;
        while (!pathExists(parentPath)) {
            parentPath = getParentDirectoryPath(parentPath);
        }

        std::string pathTail;
        if (parentPath == ".")
            pathTail = path;
        else
            pathTail = path.substr(parentPath.size(), path.size() - parentPath.size());

        char *absolutePathTemp = realpath(parentPath.c_str(), NULL);
        if (absolutePathTemp != NULL) {
            std::string absoluteParentPath(absolutePathTemp);
            free(absolutePathTemp);
            if (pathTail.size() == 0)
                return absoluteParentPath;
            else
                return absoluteParentPath + DataStringGlobals::pathChar + pathTail;

        } else {
            std::cout << "ERROR: Could not resolve path for " + path + "." << std::endl;
            std::exit(EXIT_FAILURE);
        }
#endif
    }

    std::string getProgramPath()
    {
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

    std::string getFileExtension(std::string const &fileName)
    {
        int extensionPosition = fileName.find_last_of(".");
        return fileName.substr(extensionPosition + 1, fileName.size() - 1);
    }

    std::string removeFileExtension(std::string const &fileName)
    {
        int extensionPosition = fileName.find_last_of(".");
        return fileName.substr(0, extensionPosition);
    }

    void makeDirectory(std::string const &directoryPath)
    {
        // Create a directory if doesn't already exist
        if (pathExists(directoryPath)) { // path already exists
            if (!directoryExists(directoryPath)) {
                std::cout << "ERROR: " + getAbsolutePath(directoryPath) + " is not a directory." << std::endl;
                std::exit(EXIT_FAILURE);
            }
        } else { // directory does not already exist
            std::string parentDirectoryPath = getParentDirectoryPath(directoryPath);
            if (!pathExists(parentDirectoryPath)) {
                std::cout << "ERROR: " + getAbsolutePath(parentDirectoryPath) + " is not a directory." << std::endl;
                std::exit(EXIT_FAILURE);
            }
#ifdef _WIN32
            CreateDirectory(directoryPath.c_str(), NULL);
#else
            mkdir(directoryPath.c_str(), 0755);
#endif
        }
    }

    bool pathExists(std::string const &path)
    {
#ifdef _WIN32
        return PathFileExists(path.c_str());
#else
        struct stat info;
        return (stat(path.c_str(), &info) == 0);
#endif
    }

    bool directoryExists(std::string const &directoryPath)
    {
#ifdef _WIN32
        if (PathFileExists(directoryPath.c_str()))
            return PathIsDirectory(directoryPath.c_str());
        else
            return false;
#else
        struct stat info;
        if (stat(directoryPath.c_str(), &info) == 0) {
            return (info.st_mode & S_IFDIR);
        } else
            return false;
#endif
    }

    bool fileExists(std::string const &filePath)
    {
#ifdef _WIN32
        if (PathFileExists(filePath.c_str()))
            return !PathIsDirectory(filePath.c_str());
        else
            return false;
#else
        struct stat info;
        if (stat(filePath.c_str(), &info) == 0) {
            return !(info.st_mode & S_IFDIR);
        } else
            return false;
#endif
    }

    void moveFile(std::string const &filePath, std::string const &destination)
    {
        rename(filePath.c_str(), destination.c_str());
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
        remove(fileName.c_str());
    }

    void linkFile(std::string const &fileName, std::string const &link)
    {
#ifdef _WIN32
        CopyFile(fileName.c_str(), link.c_str(), false);
#else
        int returnValue = symlink(fileName.c_str(), link.c_str()); // ignore the return value
        // we want to ignore the return value without muting all warnings, so...
        (void)(returnValue + 1); // outsmart the compiler :-/
#endif
    }

} // namespace FileSystem
} // namespace EnergyPlus
