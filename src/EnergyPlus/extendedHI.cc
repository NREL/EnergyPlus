// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

// Adapted from the code Version 1.1 released by Yi-Chuan Lu on February 23, 2023.
//
// @article{20heatindex,
//   Title   = {Extending the Heat Index},
//   Author  = {Yi-Chuan Lu and David M. Romps},
//   Journal = {Journal of Applied Meteorology and Climatology},
//   Year    = {2022},
//   Volume  = {61},
//   Number  = {10},
//   Pages   = {1367--1383},
//   Year    = {2022},
// }
//
// This headindex function returns the Heat Index in Kelvin. The inputs are:
// - T, the temperature in Kelvin
// - RH, the relative humidity, which is a value from 0 to 1
// - show_info is an optional logical flag. If true, the function returns the physiological state.

#include <cmath>

#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HVACSystemRootFindingAlgorithm.hh>
#include <EnergyPlus/extendedHI.hh>

namespace EnergyPlus {

namespace extendedHI {
    // Thermodynamic parameters
    constexpr Real64 Ttrip = 273.16; // K
    constexpr Real64 ptrip = 611.65; // Pa
    constexpr Real64 E0v = 2.3740e6; // J/kg
    constexpr Real64 E0s = 0.3337e6; // J/kg
    constexpr Real64 rgasa = 287.04; // J/kg/K
    constexpr Real64 rgasv = 461.;   // J/kg/K
    constexpr Real64 cva = 719.;     // J/kg/K
    constexpr Real64 cvv = 1418.;    // J/kg/K
    constexpr Real64 cvl = 4119.;    // J/kg/K
    constexpr Real64 cvs = 1861.;    // J/kg/K
    constexpr Real64 cpa = cva + rgasa;
    constexpr Real64 cpv = cvv + rgasv;

    // The saturation vapor pressure
    Real64 pvstar(Real64 T)
    {
        if (T == 0.0) {
            return 0.0;
        } else if (T < Ttrip) {
            return ptrip * pow((T / Ttrip), ((cpv - cvs) / rgasv)) * exp((E0v + E0s - (cvv - cvs) * Ttrip) / rgasv * (1. / Ttrip - 1. / T));
        } else {
            return ptrip * pow((T / Ttrip), ((cpv - cvl) / rgasv)) * exp((E0v - (cvv - cvl) * Ttrip) / rgasv * (1. / Ttrip - 1. / T));
        }
        return 0.0;
    }

    // The latent heat of vaporization of water
    Real64 Le(Real64 T)
    {
        return (E0v + (cvv - cvl) * (T - Ttrip) + rgasv * T);
    }

    // Thermoregulatory parameters
    constexpr Real64 sigma = 5.67e-8;                 // W/m^2/K^4 , Stefan-Boltzmann constant
    constexpr Real64 epsilon = 0.97;                  //           , emissivity of surface, steadman1979
    constexpr Real64 M = 83.6;                        // kg        , mass of average US adults, fryar2018
    constexpr Real64 H = 1.69;                        // m         , height of average US adults, fryar2018
    Real64 A = 0.202 * pow(M, 0.425) * pow(H, 0.725); // m^2       , DuBois formula, parson2014
    constexpr Real64 cpc = 3492.;                     // J/kg/K    , specific heat capacity of core, gagge1972
    Real64 C = M * cpc / A;                           //           , heat capacity of core
    constexpr Real64 r = 124.;                        // Pa/K      , Zf/Rf, steadman1979
    constexpr Real64 Q = 180.;                        // W/m^2     , metabolic rate per skin area, steadman1979
    constexpr Real64 phi_salt = 0.9;                  //           , vapor saturation pressure level of saline solution, steadman1979
    constexpr Real64 Tc = 310.;                       // K         , core temperature, steadman1979
    Real64 Pc = phi_salt * pvstar(Tc);                //           , core vapor pressure
    Real64 L = Le(310.);                              //           , latent heat of vaporization at 310 K
    constexpr Real64 p = 1.013e5;                     // Pa        , atmospheric pressure
    constexpr Real64 eta = 1.43e-6;                   // kg/J      , "inhaled mass" / "metabolic rate", steadman1979
    constexpr Real64 Pa0 = 1.6e3;                     // Pa        , reference air vapor pressure in regions III, IV, V, VI, steadman1979

    // Function to calculate respiratory heat loss, W/m^2
    Real64 Qv(Real64 Ta, Real64 Pa)
    {
        return eta * Q * (cpa * (Tc - Ta) + L * rgasa / (p * rgasv) * (Pc - Pa));
    }

    // Function to calculate mass transfer resistance through skin, Pa m^2/W
    Real64 Zs(Real64 Rs)
    {
        return (Rs == 0.0387) ? 52.1 : 6.0e8 * std::pow(Rs, 5);
    }

    // Function to calculate heat transfer resistance through air, exposed part of skin, K m^2/W
    Real64 Ra(Real64 Ts, Real64 Ta)
    {
        constexpr Real64 hc = 17.4;
        constexpr Real64 phi_rad = 0.85;
        Real64 hr = epsilon * phi_rad * sigma * (std::pow(Ts, 2) + std::pow(Ta, 2)) * (Ts + Ta);
        return 1.0 / (hc + hr);
    }

    // Function to calculate heat transfer resistance through air, clothed part of skin, K m^2/W
    Real64 Ra_bar(Real64 Tf, Real64 Ta)
    {
        constexpr Real64 hc = 11.6;
        constexpr Real64 phi_rad = 0.79;
        Real64 hr = epsilon * phi_rad * sigma * (std::pow(Tf, 2) + std::pow(Ta, 2)) * (Tf + Ta);
        return 1.0 / (hc + hr);
    }

    // Function to calculate heat transfer resistance through air, when being naked, K m^2/W
    Real64 Ra_un(Real64 Ts, Real64 Ta)
    {
        constexpr Real64 hc = 12.3;
        constexpr Real64 phi_rad = 0.80;
        Real64 hr = epsilon * phi_rad * sigma * (std::pow(Ts, 2) + std::pow(Ta, 2)) * (Ts + Ta);
        return 1.0 / (hc + hr);
    }

    constexpr Real64 Za = 60.6 / 17.4;     // Pa m^2/W, mass transfer resistance through air, exposed part of skin
    constexpr Real64 Za_bar = 60.6 / 11.6; // Pa m^2/W, mass transfer resistance through air, clothed part of skin
    constexpr Real64 Za_un = 60.6 / 12.3;  // Pa m^2/W, mass transfer resistance through air, when being naked

    constexpr Real64 tol = 1e-8;
    constexpr Real64 tolT = 1e-8;
    constexpr Real64 maxIter = 100;

    std::tuple<std::string, Real64, Real64, Real64, Real64> find_eqvar(EnergyPlusData &state, Real64 Ta, Real64 RH)
    {
        Real64 Pa = RH * pvstar(Ta);
        Real64 Rs = 0.0387;
        Real64 phi = 0.84;
        Real64 dTcdt = 0.0;
        Real64 m = (Pc - Pa) / (Zs(Rs) + Za);
        Real64 m_bar = (Pc - Pa) / (Zs(Rs) + Za_bar);

        int SolFla;
        Real64 Ts;
        General::SolveRoot(
            state,
            tol,
            maxIter,
            SolFla,
            Ts,
            [&](Real64 Ts) { return (Ts - Ta) / Ra(Ts, Ta) + (Pc - Pa) / (Zs(Rs) + Za) - (Tc - Ts) / Rs; },
            std::max(0.0, std::min(Tc, Ta) - Rs * std::abs(m)),
            std::max(Tc, Ta) + Rs * std::abs(m));

        Real64 Tf;
        General::SolveRoot(
            state,
            tol,
            maxIter,
            SolFla,
            Tf,
            [&](Real64 Tf) { return (Tf - Ta) / Ra_bar(Tf, Ta) + (Pc - Pa) / (Zs(Rs) + Za_bar) - (Tc - Tf) / Rs; },
            std::max(0.0, std::min(Tc, Ta) - Rs * std::abs(m_bar)),
            std::max(Tc, Ta) + Rs * std::abs(m_bar));
        Real64 flux1 = Q - Qv(Ta, Pa) - (1.0 - phi) * (Tc - Ts) / Rs;
        Real64 flux2 = Q - Qv(Ta, Pa) - (1.0 - phi) * (Tc - Ts) / Rs - phi * (Tc - Tf) / Rs;
        std::string eqvar_name;
        Real64 Rf;

        if (flux1 <= 0.0) {
            eqvar_name = "phi";
            phi = 1.0 - (Q - Qv(Ta, Pa)) * Rs / (Tc - Ts);
            Rf = std::numeric_limits<Real64>::infinity();
        } else if (flux2 <= 0.0) {
            eqvar_name = "Rf";
            Real64 Ts_bar = Tc - (Q - Qv(Ta, Pa)) * Rs / phi + (1.0 / phi - 1.0) * (Tc - Ts);
            General::SolveRoot(
                state,
                tol,
                maxIter,
                SolFla,
                Tf,
                [&](Real64 Tf) {
                    return (Tf - Ta) / Ra_bar(Tf, Ta) + (Pc - Pa) * (Tf - Ta) / ((Zs(Rs) + Za_bar) * (Tf - Ta) + r * Ra_bar(Tf, Ta) * (Ts_bar - Tf)) -
                           (Tc - Ts_bar) / Rs;
                },
                Ta,
                Ts_bar);
            Rf = Ra_bar(Tf, Ta) * (Ts_bar - Tf) / (Tf - Ta);
        } else {
            Rf = 0.0;
            Real64 flux3 = Q - Qv(Ta, Pa) - (Tc - Ta) / Ra_un(Tc, Ta) - (phi_salt * pvstar(Tc) - Pa) / Za_un;
            if (flux3 < 0.0) {
                General::SolveRoot(
                    state,
                    tol,
                    maxIter,
                    SolFla,
                    Ts,
                    [&](Real64 Ts) { return (Ts - Ta) / Ra_un(Ts, Ta) + (Pc - Pa) / (Zs((Tc - Ts) / (Q - Qv(Ta, Pa))) + Za_un) - (Q - Qv(Ta, Pa)); },
                    0.0,
                    Tc);
                Rs = (Tc - Ts) / (Q - Qv(Ta, Pa));
                eqvar_name = "Rs";
                Real64 Ps = Pc - (Pc - Pa) * Zs(Rs) / (Zs(Rs) + Za_un);
                if (Ps > phi_salt * pvstar(Ts)) {
                    General::SolveRoot(
                        state,
                        tol,
                        maxIter,
                        SolFla,
                        Ts,
                        [&](Real64 Ts) { return (Ts - Ta) / Ra_un(Ts, Ta) + (phi_salt * pvstar(Ts) - Pa) / Za_un - (Q - Qv(Ta, Pa)); },
                        0.0,
                        Tc);
                    Rs = (Tc - Ts) / (Q - Qv(Ta, Pa));
                    eqvar_name = "Rs*";
                }
            } else {
                Rs = 0.0;
                eqvar_name = "dTcdt";
                dTcdt = (1.0 / C) * flux3;
            }
        }
        return {eqvar_name, phi, Rf, Rs, dTcdt};
    }

    // Convert the find_T function
    std::pair<Real64, std::string> find_T(EnergyPlusData &state, std::string eqvar_name, Real64 eqvar)
    {
        Real64 T;
        std::string region;
        int SolFla;

        if (eqvar_name == "phi") {
            General::SolveRoot(
                state, tol, maxIter, SolFla, T, [&](Real64 T) { return std::get<1>(find_eqvar(state, T, 1.0)) - eqvar; }, 0.0, 240.0);
            region = "I";
        } else if (eqvar_name == "Rf") {
            General::SolveRoot(
                state,
                tol,
                maxIter,
                SolFla,
                T,
                [&](Real64 T) { return std::get<2>(find_eqvar(state, T, std::min(1.0, Pa0 / pvstar(T)))) - eqvar; },
                230.0,
                300.0);
            region = (Pa0 > pvstar(T)) ? "II" : "III";
        } else if (eqvar_name == "Rs" || eqvar_name == "Rs*") {
            General::SolveRoot(
                state, tol, maxIter, SolFla, T, [&](Real64 T) { return std::get<3>(find_eqvar(state, T, Pa0 / pvstar(T))) - eqvar; }, 295.0, 350.0);
            region = (eqvar_name == "Rs") ? "IV" : "V";
        } else {
            General::SolveRoot(
                state, tol, maxIter, SolFla, T, [&](Real64 T) { return std::get<4>(find_eqvar(state, T, Pa0 / pvstar(T))) - eqvar; }, 340.0, 1000.0);
            region = "VI";
        }

        return std::make_pair(T, region);
    }

    Real64 heatindex(EnergyPlusData &state, Real64 Ta, Real64 RH, bool show_info = false)
    {

        // Ta: temperature in Kelvin
        // RH: relative humidity in range of 0.0 to 1.0
        // show_info: whether to print some messages. Might be useful in debugging
        // The function computes the extended heat index, in Kelvinn

        auto const HVACSystemRootSolverBackup = state.dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver;
        state.dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver = HVACSystemRootSolverAlgorithm::Bisection;
        // Dictionary to map eqvar_name to tuple index
        std::map<std::string, int> dic = {{"phi", 1}, {"Rf", 2}, {"Rs", 3}, {"Rs*", 3}, {"dTcdt", 4}};
        auto eqvars = find_eqvar(state, Ta, RH);
        std::string eqvar_name = std::get<0>(eqvars);
        Real64 eqvar_value;
        if (eqvar_name == "phi") {
            eqvar_value = std::get<1>(eqvars);
        } else if (eqvar_name == "Rf") {
            eqvar_value = std::get<2>(eqvars);
        } else if (eqvar_name == "Rs" || eqvar_name == "Rs*") {
            eqvar_value = std::get<3>(eqvars);
        } else if (eqvar_name == "dTcdt") {
            eqvar_value = std::get<4>(eqvars);
        }

        auto result = find_T(state, eqvar_name, eqvar_value);
        Real64 T = std::get<0>(result);
        std::string region = std::get<1>(result);

        if (Ta == 0.0) T = 0.0;

        if (show_info) {
            if (region == "I") {
                std::cout << "Region I, covering (variable phi)\n";
                std::cout << "Clothing fraction is " << std::round(std::get<1>(eqvars) * 1000.0) / 1000.0 << "\n";
            } else if (region == "II") {
                std::cout << "Region II, clothed (variable Rf, pa = pvstar)\n";
                std::cout << "Clothing thickness is " << std::round((std::get<2>(eqvars) / 16.7) * 100.0 * 1000.0) / 1000.0 << " cm\n";
            } else if (region == "III") {
                std::cout << "Region III, clothed (variable Rf, pa = pref)\n";
                std::cout << "Clothing thickness is " << std::round((std::get<2>(eqvars) / 16.7) * 100.0 * 1000.0) / 1000.0 << " cm\n";
            } else if (region == "IV") {
                Real64 kmin = 5.28; // W/K/m^2, conductance of tissue
                Real64 rho = 1.0e3; // kg/m^3, density of blood
                Real64 c = 4184.0;  // J/kg/K, specific heat of blood
                std::cout << "Region IV, naked (variable Rs, ps < phisalt*pvstar)\n";
                std::cout << "Blood flow is " << std::round(((1.0 / std::get<3>(eqvars) - kmin) * A / (rho * c)) * 1000.0 * 60.0 * 1000.0) / 1000.0
                          << " l/min\n";
            } else if (region == "V") {
                Real64 kmin = 5.28; // W/K/m^2, conductance of tissue
                Real64 rho = 1.0e3; // kg/m^3, density of blood
                Real64 c = 4184.0;  // J/kg/K, specific heat of blood
                std::cout << "Region V, naked dripping sweat (variable Rs, ps = phisalt*pvstar)\n";
                std::cout << "Blood flow is " << std::round(((1.0 / std::get<3>(eqvars) - kmin) * A / (rho * c)) * 1000.0 * 60.0 * 1000.0) / 1000.0
                          << " l/min\n";
            } else {
                std::cout << "Region VI, warming up (dTc/dt > 0)\n";
                std::cout << "dTc/dt = " << std::round(std::get<4>(eqvars) * 3600.0 * 1000000.0) / 1000000.0 << " K/hour\n";
            }
        }

        state.dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver = HVACSystemRootSolverBackup;
        return T;
    }

} // namespace extendedHI
} // namespace EnergyPlus