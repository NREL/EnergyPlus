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

#ifndef ENERGYPLUS_CACHE_HH
#define ENERGYPLUS_CACHE_HH

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/UtilityRoutines.hh>

// JSON Header
#include <nlohmann/json.hpp>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace Cache {

    std::string const cacheName = "eplusout.cache";
    std::string const CTFKey = "CTFs";

    void readJSONfile(EnergyPlusData &state, fs::path const &filePath, nlohmann::json &j);

    void writeJSONfile(nlohmann::json &j, fs::path const &filePath);

    template <typename T> void jsonToArray(EnergyPlusData &state, Array1D<T> &arr, nlohmann::json &j, std::string const &key)
    {
        // 0-based array to JSON list

        try {
            int size = static_cast<int>(j.at(key).size());
            arr.dimension({0, size - 1});
            int idx = 0;
            for (auto &v : j.at(key)) {
                arr(idx) = v.get<T>();
                ++idx;
            }
        } catch (const nlohmann::json::out_of_range &e) {
            ShowFatalError(state, format("From eplusout.cache, key: \"{}\" not found", key));
        }
    }

    template <typename T> void jsonToArray1(EnergyPlusData &state, Array1D<T> &arr, nlohmann::json &j, std::string const &key)
    {
        // 1-based array to JSON list

        try {
            int size = static_cast<int>(j.at(key).size());
            arr.dimension(size);
            int idx = 1;
            for (auto &v : j.at(key)) {
                arr(idx) = v.get<T>();
                ++idx;
            }
        } catch (const nlohmann::json::out_of_range &e) {
            ShowFatalError(state, format("From eplusout.cache, key: \"{}\" not found", key));
        }
    }

    template <typename T> void jsonToData(EnergyPlusData &state, T &data, nlohmann::json &j, std::string const &key)
    {
        try {
            data = j.at(key).get<T>();
        } catch (const nlohmann::json::out_of_range &e) {
            ShowFatalError(state, format("From eplusout.cache, key: \"{}\" not found", key));
        }
    }

    template <typename T> void arrayToJSON(Array1D<T> const &arr, nlohmann::json &j, std::string const &key)
    {
        std::vector<T> vect;
        for (auto &v : arr)
            vect.push_back(v);
        j[key] = vect;
    }

    void loadCache(EnergyPlusData &state);

    void writeCache(EnergyPlusData &state);

    inline unsigned long long prepFloatForCacheKey(double const x, unsigned int const precision_bits)
    {
        assert(sizeof(double) == sizeof(unsigned long long));
        return *reinterpret_cast<unsigned long long const *>(&x) >> (64 - 12 - precision_bits);
    }

} // namespace Cache

struct CacheData : BaseGlobalStruct
{

    nlohmann::json cache;
    bool ctfObjectsInCache = false;
    std::unordered_map<std::string, nlohmann::json> unorderedCTFObjects;

    void clear_state() override
    {
        this->cache.clear();
        this->ctfObjectsInCache = false;
        this->unorderedCTFObjects.clear();
    }
};

} // namespace EnergyPlus

#endif // ENERGYPLUS_CACHE_HH
