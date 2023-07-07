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

#include <array>       // for array
#include <clocale>     // for setlocale, LC_ALL
#include <cwchar>      // for mbsrtowcs, wcsrtombs, wcslen, mbstate_t
#include <filesystem>  // for path
#include <stdexcept>   // for runtime_error
#include <string>      // for string, wstring, operator+, to_string
#include <type_traits> // for is_same_v
#include <utility>     // for forward

namespace CLI {
namespace detail {

    /// Attempt to set one of the acceptable unicode locales for conversion
    inline void set_unicode_locale()
    {
        static const std::array<const char *, 3> unicode_locales{{"C.UTF-8", "en_US.UTF-8", ".UTF-8"}};

        for (const auto &locale_name : unicode_locales) {
            if (std::setlocale(LC_ALL, locale_name) != nullptr) {
                return;
            }
        }
        throw std::runtime_error("CLI::narrow: could not set locale to C.UTF-8");
    }

    template <typename F> struct scope_guard_t
    {
        F closure;

        explicit scope_guard_t(F closure_) : closure(closure_)
        {
        }
        ~scope_guard_t()
        {
            closure();
        }
    };

    // simple scope guard object, employs RAII to execute a callback when leaving scope, no matter if it's from a return or an exception
    template <typename F> inline scope_guard_t<F> scope_guard(F &&closure)
    {
        return scope_guard_t<F>{std::forward<F>(closure)};
    }

    inline std::string narrow_impl(const wchar_t *str, std::size_t str_size)
    {
        (void)str_size;
        std::mbstate_t state = std::mbstate_t();
        const wchar_t *it = str;

        std::string old_locale = std::setlocale(LC_ALL, nullptr);                   // get current locale
        auto sg = scope_guard([&] { std::setlocale(LC_ALL, old_locale.c_str()); }); // ensure we restore it even if we throw
        set_unicode_locale();

        std::size_t new_size = std::wcsrtombs(nullptr, &it, 0, &state);
        if (new_size == static_cast<std::size_t>(-1)) {
            throw std::runtime_error("CLI::narrow: conversion error in std::wcsrtombs at offset " + std::to_string(it - str));
        }
        std::string result(new_size, '\0');
        std::wcsrtombs(const_cast<char *>(result.data()), &str, new_size, &state);

        return result;
    }

    inline std::wstring widen_impl(const char *str, std::size_t str_size)
    {
        (void)str_size;
        std::mbstate_t state = std::mbstate_t();
        const char *it = str;

        std::string old_locale = std::setlocale(LC_ALL, nullptr);
        auto sg = scope_guard([&] { std::setlocale(LC_ALL, old_locale.c_str()); });
        set_unicode_locale();

        const std::size_t new_size = std::mbsrtowcs(nullptr, &it, 0, &state);
        if (new_size == static_cast<std::size_t>(-1)) {
            throw std::runtime_error("CLI::widen: conversion error in std::mbsrtowcs at offset " + std::to_string(it - str));
        }
        std::wstring result(new_size, L'\0');
        std::mbsrtowcs(const_cast<wchar_t *>(result.data()), &str, new_size, &state);

        return result;
    }
} // namespace detail

inline std::string narrow(const wchar_t *str)
{
    return detail::narrow_impl(str, std::wcslen(str));
}

inline std::string narrow(const std::wstring &str)
{
    return detail::narrow_impl(str.data(), str.size());
}

inline std::wstring widen(const std::string &str)
{
    return detail::widen_impl(str.data(), str.size());
}
inline std::wstring widen(const char *str)
{
    return detail::widen_impl(str, std::strlen(str));
}

inline std::filesystem::path to_path(const std::string &str)
{
    if constexpr (std::is_same_v<typename std::filesystem::path::value_type, wchar_t>) {
        return {widen(str)};
    } else {
        return {str};
    }
}
} // namespace CLI
