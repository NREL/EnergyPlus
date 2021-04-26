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

// EnergyPlus Headers
#include <EnergyPlus/Cache.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::Cache {

void readJSONfile(EnergyPlusData &state, std::string &filePath, nlohmann::json &j)
{
    if (!FileSystem::fileExists(filePath)) {
        // if the file doesn't exist, there are no data to read
        return;
    } else {
        std::ifstream ifs(filePath);

        // read json_in data
        try {
            ifs >> j;
            ifs.close();
        } catch (...) {
            if (!j.empty()) {
                // file exists, is not empty, but failed for some other reason
                ShowWarningError(state, filePath + " contains invalid file format");
            }
            ifs.close();
            return;
        }
    }
}

void writeJSONfile(nlohmann::json &j, std::string &fPath)
{
    std::ofstream ofs(fPath);
    ofs << std::setw(2) << j;
    ofs.close();
}

void jsonToArray(EnergyPlusData &state, Array1D<Real64> &arr, nlohmann::json &j, std::string const &key)
{
    // 0-based array to JSON list

    try {
        int size = static_cast<int>(j[key].size());
        arr.dimension({0, size - 1}, 0.0);
        int idx = 0;
        for (auto &v : j[key]) {
            arr[idx] = v;
            ++idx;
        }
    } catch (nlohmann::json::out_of_range &e) {
        ShowFatalError(state, format(R"(From eplusout.cache, key: "{}" not found)", key));
    }
}

void jsonToArray1(EnergyPlusData &state, Array1D<Real64> &arr, nlohmann::json &j, std::string const &key)
{
    // 1-based array to JSON list

    try {
        int size = static_cast<int>(j[key].size());
        arr.dimension(size, 0.0);
        int idx = 0;
        for (auto &v : j[key]) {
            arr[idx] = v;
            ++idx;
        }
    } catch (nlohmann::json::out_of_range &e) {
        ShowFatalError(state, format(R"(From eplusout.cache, key: "{}" not found)", key));
    }
}

void arrayToJSON(Array1D<Real64> const &arr, nlohmann::json &j, std::string const &key)
{
    std::vector<Real64> vect;
    for (auto v : arr)
        vect.push_back(v);
    j[key] = vect;
}

void loadCache(EnergyPlusData &state)
{
    // load cache file if it exists
    if (FileSystem::fileExists(state.dataCache->cacheFilePath)) {
        state.dataCache->cacheExists = true;
        readJSONfile(state, state.dataCache->cacheFilePath, state.dataCache->cache);
    }
}

void writeCache(EnergyPlusData &state)
{
    writeJSONfile(state.dataCache->cache, state.dataCache->cacheFilePath);
}

} // namespace EnergyPlus::Cache
