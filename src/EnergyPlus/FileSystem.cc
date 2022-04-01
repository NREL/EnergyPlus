// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
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
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace FileSystem {

#ifdef _WIN32
    std::string const exeExtension(".exe");
#else
    std::string const exeExtension;
#endif

    fs::path makeNativePath(fs::path const &path)
    {
        // path.make_preferred() on windows will change "/" to "\\", because '/' is a fallback separator
        // but on Unix it will *not* change "\\" to "/", because POSIX doesn't define it as a fallback separator. In fact, it's even allowed in a
        // filename. Do we really need that though?
        // path.make_preferred();
        fs::path result = path;
        std::string tempPathAsStr = result.make_preferred().string();
        std::replace(tempPathAsStr.begin(), tempPathAsStr.end(), DataStringGlobals::altpathChar, DataStringGlobals::pathChar);
        result = fs::path(tempPathAsStr);
        return result;
    }

    // TODO: remove as providing little benefit over calling fs::path::filename directly?
    fs::path getFileName(fs::path const &filePath)
    {
        return filePath.filename();
    }

    fs::path getParentDirectoryPath(fs::path const &path)
    {
        // Note: this is needed because "/a/b/c".parent_path() = "/a/b/c/"
        std::string pathStr = path.string();
        if (!pathStr.empty()) {
            while ((pathStr.back() == DataStringGlobals::pathChar) || (pathStr.back() == DataStringGlobals::altpathChar)) {
                pathStr.erase(pathStr.size() - 1);
            }
        }

        // If empty, return "./" instead
        fs::path parent_path = fs::path(pathStr).parent_path();
        if (parent_path.empty()) {
            parent_path = "./";
        }
        return parent_path;
    }

    fs::path getAbsolutePath(fs::path const &path)
    {
        //        /*
        //         * Returns the absolute path for a given relative path.
        //         *
        //         * If the relative path points to a symlink, the symlink will
        //         * be resolved, and this function will return the absolute path
        //         * of the link.
        //         */

        // Not available in experimental/filesystem
        // return fs::weakly_canonical(fs::absolute(p));

        fs::path p = fs::absolute(path);

        while (fs::is_symlink(p)) {
            auto linkpath = fs::read_symlink(p);
            if (linkpath.is_absolute()) {
                p = linkpath;
            } else {
                // Note: temp will end up absolute but not canonical yet
                // eg:
                // temp="/home/a_folder/a_symlink"
                // linkpath="../another_folder/a_file"
                p = p.parent_path() / linkpath;
                // eg: temp ="/home/a_folder/../another_folder/a_file"
            }
        }

        fs::path result;
        // `p` now is absolute, but it isn't necessarilly canonical.
        // If you have <filesystem>, you can use `fs::weakly_canonical`. <experimental/filesystem> does **not** have `weakly_canonical` though
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
        // TODO: is this wanted?
        // The problem is really only for unit tests, if you try to compare getAbsolutePath("sandbox/") == getAbsolutePath("sandbox") you get false
        // because one has the trailing sep, the other doesn't. Both paths have the same components though (if iterated on), and as long as you use
        // path operations (such as operator/) and not string concatenation, this works just fine.
        // std::string s = result.string();
        // if (fs::is_directory(s)) {
        // s += '/';
        //}
        return result;
    }

    fs::path getProgramPath()
    {
        // /*
        // * Returns the relative path to the executable file (including symlinks).
        // *
        // * To resolve symlinks, wrap this call in getAbsolutePath().
        // */
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

        return executableRelativePath;
    }

    // TODO: remove? seems like fs::path::extension would do fine. It's only used in CommandLineInterface to check the input file type, so we could
    // just compare to ".EPJSON" instead of "EPJSON"...
    fs::path getFileExtension(fs::path const &filePath)
    {
        std::string pext = fs::path(filePath).extension().string();
        if (!pext.empty()) {
            // remove '.'
            pext = std::string(++pext.begin(), pext.end());
        }
        return fs::path{pext};
    }

    FileTypes getFileType(fs::path const &filePath)
    {
#ifdef _WIN32
        auto const filePathStr = fs::path(filePath).extension().string();
        auto extension = std::string_view(filePathStr.c_str());
#else
        auto extension = std::string_view(fs::path(filePath).extension().c_str());
#endif

        extension.remove_prefix(extension.find_last_of('.') + 1);
        return static_cast<FileTypes>(getEnumerationValue(FileTypesExt, extension));
    }

    // TODO: remove for fs::path::replace_extension directly? Note that replace_extension mutates the object
    fs::path removeFileExtension(fs::path const &filePath)
    {
        // return fs::path(filePath).stem().string();
        return fs::path(filePath).replace_extension();
    }

    fs::path replaceFileExtension(fs::path const &filePath, fs::path const &ext)
    {
        // return fs::path(filePath).stem().string();
        return fs::path(filePath).replace_extension(ext);
    }

    // TODO: remove? `fs::create_directory` for a single or `fs::create_directories` for nested directory creation
    void makeDirectory(fs::path const &directoryPath)
    {
        // Create a directory if doesn't already exist
        if (pathExists(directoryPath)) { // path already exists
            if (!directoryExists(directoryPath)) {
                std::cout << "ERROR: " << getAbsolutePath(directoryPath).string() << " already exists and is not a directory." << std::endl;
                std::exit(EXIT_FAILURE);
            }
        } else { // directory does not already exist
            // Create_directories is recursive, create_directory isn't. I don't see why we wouldn't want recursive
            fs::create_directories(directoryPath);
        }
    }

    // TODO: remove?
    bool pathExists(fs::path const &path)
    {
        return fs::exists(path);
    }

    // TODO: I think we want to keep this one
    bool directoryExists(fs::path const &directoryPath)
    {
        return fs::exists(directoryPath) && fs::is_directory(directoryPath);
    }

    bool fileExists(fs::path const &filePath)
    {
        return fs::exists(filePath) && !fs::is_directory(filePath);
    }

    void moveFile(fs::path const &filePath, fs::path const &destination)
    {
        // TODO: should we throw? Or return false?
        if (!fileExists(filePath)) {
            return;
        }

        // rename would fail if copying across devices
        try {
            fs::rename(fs::path(filePath), destination);
        } catch (fs::filesystem_error &) {
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

    bool removeFile(fs::path const &filePath)
    {
        if (!fileExists(filePath)) {
            return false;
        }

        return fs::remove(filePath);
    }

    void linkFile(fs::path const &filePath, fs::path const &linkPath)
    {
        if (!fileExists(filePath)) {
            return;
        }

#ifdef _WIN32
        fs::copy(filePath, linkPath, fs::copy_options::update_existing);
#else
        // we could return bool?
        fs::create_symlink(filePath, linkPath);
#endif
    }

    std::string readFile(fs::path const &filePath, std::ios_base::openmode mode)
    {
#ifdef _WIN32
        auto filePathStr = filePath.string();
        auto path = filePathStr.c_str();
#else
        auto path = filePath.c_str();
#endif

        if (!fileExists(filePath)) {
            throw FatalError(fmt::format("File does not exists: {}", path));
        }

        std::string_view fopen_mode;
        if (mode == std::ios_base::in) {
            fopen_mode = "r";
        } else if (mode == std::ios_base::binary) {
            fopen_mode = "b";
        } else if (mode == (std::ios_base::in | std::ios_base::binary)) {
            fopen_mode = "rb";
        } else {
            throw FatalError(fmt::format("ERROR - readFile: Bad openmode argument. Must be std::ios_base::in or std::ios_base::binary"));
        }

        auto close_file = [](FILE *f) { fclose(f); };
        auto holder = std::unique_ptr<FILE, decltype(close_file)>(fopen(path, fopen_mode.data()), close_file);
        if (!holder) {
            throw FatalError(fmt::format("Could not open file: {}", path));
        }

        auto f = holder.get();
        const auto size = fs::file_size(filePath);
        std::string result;
        result.resize(size);

        auto bytes_read = fread(result.data(), 1, size, f);
        auto is_eof = feof(f);
        auto has_error = ferror(f);
        if (is_eof != 0) {
            return result;
        }
        if (has_error != 0 || bytes_read != size) {
            throw FatalError(fmt::format("Error reading file: {}", path));
        }
        return result;
    }

    nlohmann::json readJSON(fs::path const &filePath, std::ios_base::openmode mode)
    {
#ifdef _WIN32
        auto filePathStr = filePath.string();
        auto path = filePathStr.c_str();
#else
        auto path = filePath.c_str();
#endif

        if (!fileExists(filePath)) {
            throw FatalError(fmt::format("File does not exists: {}", path));
        }

        std::string_view fopen_mode;
        if (mode == std::ios_base::in) {
            fopen_mode = "r";
        } else if (mode == std::ios_base::binary) {
            fopen_mode = "b";
        } else if (mode == (std::ios_base::in | std::ios_base::binary)) {
            fopen_mode = "rb";
        } else {
            throw FatalError(fmt::format("ERROR - readFile: Bad openmode argument. Must be std::ios_base::in or std::ios_base::binary"));
        }

        auto close_file = [](FILE *f) { fclose(f); };
        auto holder = std::unique_ptr<FILE, decltype(close_file)>(fopen(path, fopen_mode.data()), close_file);
        if (!holder) {
            throw FatalError(fmt::format("Could not open file: {}", path));
        }
        auto f = holder.get();

        auto const ext = getFileType(filePath);
        switch (ext) {
        case FileTypes::EpJSON:
        case FileTypes::JSON:
        case FileTypes::GLHE:
            return nlohmann::json::parse(f, nullptr, true, true);
        case FileTypes::CBOR:
            return nlohmann::json::from_cbor(f);
        case FileTypes::MsgPack:
            return nlohmann::json::from_msgpack(f);
        case FileTypes::UBJSON:
            return nlohmann::json::from_ubjson(f);
        case FileTypes::BSON:
            return nlohmann::json::from_bson(f);
        default:
            throw FatalError("Invalid file extension. Must be epJSON, JSON, or other experimental extensions");
        }
    }

} // namespace FileSystem
} // namespace EnergyPlus
