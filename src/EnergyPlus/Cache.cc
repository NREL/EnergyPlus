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
//#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataStringGlobals.hh>
//#include <EnergyPlus/FileSystem.hh>
//#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

//void readJSONfile(EnergyPlusData &state, fs::path const &filePath, nlohmann::json &j)
//{
//    if (!FileSystem::fileExists(filePath)) {
//        // if the file doesn't exist, there are no data to read
//        return;
//    } else {
//        std::ifstream ifs(filePath);
//
//        // read json_in data
//        try {
//            j = nlohmann::json::from_cbor(ifs);
//            ifs.close();
//        } catch (...) {
//            if (!j.empty()) {
//                // file exists, is not empty, but failed for some other reason
//                ShowWarningError(state, filePath / " contains invalid file format");
//            }
//            ifs.close();
//            return;
//        }
//    }
//}
//
//void writeJSONfile(nlohmann::json &j, fs::path const &filePath)
//{
//    std::ofstream ofs(filePath, std::ofstream::out | std::ofstream::binary);
//    nlohmann::json::to_cbor(j, ofs);
//    ofs.close();
//}

void Cache::loadCache(EnergyPlusData &state)
{
    // load cache file if it exists
    if (!state.dataGlobal->useCache) {
        return;
    }
    cache = FileSystem::readJSON(state.dataStrGlobals->outputCacheFileName);
    state.dataCache->ctfObjectsInCache = true;
//
//    if (state.dataGlobal->useCache && FileSystem::fileExists(state.dataStrGlobals->outputCacheFileName)) {
//        Cache::readJSONfile(state, state.dataStrGlobals->outputCacheFileName, state.dataCache->cache);
//
//        // file exists but is empty, so don't try to read data
//        if (state.dataCache->cache.empty()) return;
//
//        try {
//            // load these up one time up front
//            // nlohmann::json is loading this by default as an ordered set of data, but it needs to be unordered for the search later
//            nlohmann::json allCTFs = state.dataCache->cache.at(Cache::CTFKey.data());
//            allCTFs.get_to(state.dataCache->unorderedCTFObjects);
//            state.dataCache->ctfObjectsInCache = true;
//        } catch (nlohmann::json::out_of_range &e) {
//            state.dataCache->ctfObjectsInCache = false;
//        }
//    }
}

void Cache::writeCache(EnergyPlusData &state)
{
    FileSystem::writeFile<FileSystem::FileTypes::CBOR>(state.dataStrGlobals->outputCacheFileName, cache);
//    writeJSONfile(state.dataCache->cache, state.dataStrGlobals->outputCacheFileName);
}

//void Cache::findKey(nlohmann::json *input, std::string_view const key)
//{
//    auto const found = input->find(key.data());
//    if (found == cache.end()) {
//        throw FatalError(fmt::format("key: \"{}\" not found in cache", key));
//    }
//    input = &found.value();
//}

nlohmann::json *Cache::writeKey(nlohmann::json *input, std::string_view const key)
{
    auto const found = input->find(key.data());
    if (found == cache.end()) {
        return &input->emplace(key, nlohmann::json()).first.value();
//        throw FatalError(fmt::format("key: \"{}\" not found in cache", key));
    } else {
        input = &found.value();
        return input;
    }
}

} // namespace EnergyPlus::Cache
