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

#ifndef Psychrometrics_hh_INCLUDED
#define Psychrometrics_hh_INCLUDED

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/bit.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/PsychCacheData.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

#ifdef EP_nocache_Psychrometrics
#undef EP_cache_PsyTwbFnTdbWPb
#undef EP_cache_PsyPsatFnTemp
#undef EP_cache_PsyTsatFnPb
#undef EP_cache_PsyTsatFnHPb
#else
#define EP_cache_PsyTwbFnTdbWPb
#define EP_cache_PsyPsatFnTemp
#define EP_cache_PsyTsatFnPb
#define EP_cache_PsyTsatFnHPb
#endif

namespace Psychrometrics {

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // call for recurring errors
    constexpr int iPsyTdpFnTdbTwbPb = 1;
    constexpr int iPsyRhFnTdbWPb = 2;
    constexpr int iPsyTwbFnTdbWPb = 3;
    constexpr int iPsyTwbFnTdbWPb2 = 14;
    constexpr int iPsyTwbFnTdbWPb3 = 15; // convergence
    constexpr int iPsyVFnTdbWPb = 4;
    constexpr int iPsyWFnTdpPb = 5;
    constexpr int iPsyWFnTdbH = 6;
    constexpr int iPsyWFnTdbTwbPb = 7;
    constexpr int iPsyWFnTdbTwbPb2 = 16;
    constexpr int iPsyWFnTdbRhPb = 8;
    constexpr int iPsyPsatFnTemp = 9;
    constexpr int iPsyTsatFnHPb = 10;
    constexpr int iPsyTsatFnPb = 11;
    constexpr int iPsyTsatFnPb2 = 17; // iterations
    constexpr int iPsyRhFnTdbRhov = 12;
    constexpr int iPsyRhFnTdbRhovLBnd0C = 13;
    constexpr int iPsyTwbFnTdbWPb_cache = 18;
    constexpr int iPsyPsatFnTemp_cache = 19;

#ifdef EP_psych_stats
    extern Array1D_string const PsyRoutineNames; // 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 - HR | 15 - max iter | 16 - HR | 17 -
                                                 // max iter | 18 - PsyTwbFnTdbWPb_raw (raw calc) | 19 - PsyPsatFnTemp_raw (raw calc)

    extern Array1D_bool const PsyReportIt; // PsyTdpFnTdbTwbPb     1 | PsyRhFnTdbWPb        2 | PsyTwbFnTdbWPb       3 | PsyVFnTdbWPb         4 |
                                           // PsyWFnTdpPb          5 | PsyWFnTdbH           6 | PsyWFnTdbTwbPb       7 | PsyWFnTdbRhPb        8 |
                                           // PsyPsatFnTemp        9 | PsyTsatFnHPb         10 | PsyTsatFnPb          11 | PsyRhFnTdbRhov       12 |
                                           // PsyRhFnTdbRhovLBnd0C 13 | PsyTwbFnTdbWPb       14 - HR | PsyTwbFnTdbWPb       15 - max iter |
                                           // PsyWFnTdbTwbPb       16 - HR | PsyTsatFnPb          17 - max iter | PsyTwbFnTdbWPb_cache 18 -
                                           // PsyTwbFnTdbWPb_raw (raw calc) | PsyPsatFnTemp_cache  19 - PsyPsatFnTemp_raw (raw calc)
#endif

#ifndef EP_psych_errors
#endif

#ifdef EP_cache_PsyTwbFnTdbWPb
    constexpr int twbcache_size = 1024 * 1024;
    constexpr int twbprecision_bits = 20;
#endif
#ifdef EP_cache_PsyPsatFnTemp
    constexpr int psatcache_size = 1024 * 1024;
    constexpr int psatprecision_bits = 24; // 28  //24  //32
    constexpr Int64 psatcache_mask = psatcache_size - 1;
#endif
#ifdef EP_cache_PsyTsatFnPb
    constexpr int tsatcache_size = 1024 * 1024;
    constexpr int tsatprecision_bits = 24;
    constexpr Int64 tsatcache_mask = tsatcache_size - 1;
#endif
#ifdef EP_cache_PsyTsatFnHPb
    constexpr int tsat_hbp_cache_size = 1024 * 1024;
    constexpr int tsat_hbp_precision_bits = 28;
#endif

    void InitializePsychRoutines(EnergyPlusData &state);

    void ShowPsychrometricSummary(EnergyPlusData &state, InputOutputFile &auditFile);

#ifdef EP_psych_errors
    void PsyRhoAirFnPbTdbW_error(EnergyPlusData &state,
                                 Real64 const pb,                       // barometric pressure (Pascals)
                                 Real64 const tdb,                      // dry bulb temperature (Celsius)
                                 Real64 const dw,                       // humidity ratio (kgWater/kgDryAir)
                                 Real64 const rhoair,                   // density of air
                                 std::string_view const CalledFrom = "" // routine this function was called from (error messages) !unused1208
    );
#endif

    inline Real64
    PsyRhoAirFnPbTdbW([[maybe_unused]] EnergyPlusData &state,
                      Real64 const pb,                                        // barometric pressure (Pascals)
                      Real64 const tdb,                                       // dry bulb temperature (Celsius)
                      Real64 const dw,                                        // humidity ratio (kgWater/kgDryAir)
                      [[maybe_unused]] std::string_view const CalledFrom = "" // routine this function was called from (error messages) !unused1208
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         G. S. Wright
        //       DATE WRITTEN   June 2, 1994
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides density of air as a function of barometric
        // pressure, dry bulb temperature, and humidity ratio.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        //    universal gas const for air 287 J/(kg K)
        //    air/water molecular mass ratio 28.9645/18.01534

        // REFERENCES:
        // Wylan & Sontag, Fundamentals of Classical Thermodynamics.
        // ASHRAE handbook 1985 Fundamentals, Ch. 6, eqn. (6),(26)

        Real64 const rhoair(pb / (287.0 * (tdb + DataGlobalConstants::KelvinConv) * (1.0 + 1.6077687 * max(dw, 1.0e-5))));
#ifdef EP_psych_errors
        if (rhoair < 0.0) PsyRhoAirFnPbTdbW_error(state, pb, tdb, dw, rhoair, CalledFrom);
#endif
        return rhoair;
    }

    inline Real64 PsyRhoAirFnPbTdbW_fast([[maybe_unused]] EnergyPlusData &state,
                                         Real64 const pb,  // barometric pressure (Pascals)
                                         Real64 const tdb, // dry bulb temperature (Celsius)
                                         Real64 const dw   // humidity ratio (kgWater/kgDryAir)
    )
    {
        // Faster version with humidity ratio already adjusted
        assert(dw >= 1.0e-5);
        Real64 const rhoair(pb / (287.0 * (tdb + DataGlobalConstants::KelvinConv) * (1.0 + 1.6077687 * dw)));
#ifdef EP_psych_errors
        if (rhoair < 0.0) PsyRhoAirFnPbTdbW_error(state, pb, tdb, dw, rhoair);
#endif
        return rhoair;
    }

    inline Real64 PsyHfgAirFnWTdb([[maybe_unused]] Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
                                  Real64 const T                   // input temperature {Celsius}
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   May, 2001
        //       MODIFIED       June, 2002
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides latent energy of air as function of humidity ratio and temperature.

        // METHODOLOGY EMPLOYED:
        // calculates hg and then hf and the difference is Hfg.

        // REFERENCES:
        // see ASHRAE Fundamentals Psychrometric Chapter
        // USAGE:  hfg = PsyHfgAirFnWTdb(w,T)

        // Return value
        // result => heat of vaporization for moist air {J/kg}

        // This formulation currently does not use W since it returns results that are in J/kg and the
        //  amount of energy is on a per unit of moisture basis.

        Real64 const Temperature(max(T, 0.0));                               // input temperature {Celsius} - corrected for >= 0C
        return (2500940.0 + 1858.95 * Temperature) - (4180.0 * Temperature); // enthalpy of the gas - enthalpy of the fluid
    }

    inline Real64 PsyHgAirFnWTdb([[maybe_unused]] Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
                                 Real64 const T                   // input temperature {Celsius}
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   May, 2001
        //       MODIFIED       June, 2002
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides latent energy of the moisture as a gas in the air as
        // function of humidity ratio and temperature.

        // REFERENCES:
        // see ASHRAE Fundamentals Psychrometric Chapter
        // USAGE:  hg = PsyHgAirFnWTdb(w,T)

        // This formulation currently does not use W since it returns results that are in J/kg and the
        //  amount of energy is on a per unit of moisture basis.

        return 2500940.0 + 1858.95 * T; // enthalpy of the gas {units?}
    }

    inline Real64 PsyHFnTdbW(Real64 const TDB, // dry-bulb temperature {C}
                             Real64 const dW   // humidity ratio
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the enthalpy {J/kg} from dry-bulb temperature and humidity ratio.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

        // calculate enthalpy
        return 1.00484e3 * TDB + max(dW, 1.0e-5) * (2.50094e6 + 1.85895e3 * TDB); // enthalpy {J/kg}
    }

    inline Real64 PsyHFnTdbW_fast(Real64 const TDB, // dry-bulb temperature {C}
                                  Real64 const dW   // humidity ratio
    )
    {
        // Faster version with humidity ratio already adjusted
        assert(dW >= 1.0e-5);

        // calculate enthalpy
        return 1.00484e3 * TDB + dW * (2.50094e6 + 1.85895e3 * TDB); // enthalpy {J/kg}
    }

    inline Real64 PsyCpAirFnW(Real64 const dw // humidity ratio {kgWater/kgDryAir}
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         J. C. VanderZee
        //       DATE WRITTEN   Feb. 1994
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the heat capacity of air {J/kg-C} as function of humidity ratio.

        // METHODOLOGY EMPLOYED:
        // take numerical derivative of PsyHFnTdbW function

        // REFERENCES:
        // see PsyHFnTdbW ref. to ASHRAE Fundamentals
        // USAGE:  cpa = PsyCpAirFnW(w)

        // Static locals
        static Real64 dwSave(-100.0);
        static Real64 cpaSave(-100.0);

        // check if last call had the same input and if it did just use the saved output
        if (dwSave == dw) return cpaSave;

        // compute heat capacity of air
        Real64 const w(max(dw, 1.0e-5));
        Real64 const cpa((1.00484e3 + w * 1.85895e3)); // result => heat capacity of moist air {J/kg-C}

        // save values for next call
        dwSave = dw;
        cpaSave = cpa;

        return cpa;
    }

    inline Real64 PsyCpAirFnW_fast(Real64 const dw // humidity ratio {kgWater/kgDryAir}
    )
    {
        // Faster version with humidity ratio already adjusted
        assert(dw >= 1.0e-5);

        // Static locals
        static Real64 dwSave(-100.0);
        static Real64 cpaSave(-100.0);

        // check if last call had the same input and if it did just use the saved output
        if (dwSave == dw) return cpaSave;

        // compute heat capacity of air
        Real64 const cpa((1.00484e3 + dw * 1.85895e3)); // result => heat capacity of moist air {J/kg-C}

        // save values for next call
        dwSave = dw;
        cpaSave = cpa;

        return cpa;
    }

    inline Real64 PsyTdbFnHW(Real64 const H, // enthalpy {J/kg}
                             Real64 const dW // humidity ratio
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         J. C. VanderZee
        //       DATE WRITTEN   Feb. 1994
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides air temperature from enthalpy and humidity ratio.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32
        //   by inverting function PsyHFnTdbW

        Real64 const W(max(dW, 1.0e-5));                          // humidity ratio
        return (H - 2.50094e6 * W) / (1.00484e3 + 1.85895e3 * W); // result=> dry-bulb temperature {C}
    }

    inline Real64 PsyRhovFnTdbRhLBnd0C(Real64 const Tdb, // dry-bulb temperature {C}
                                       Real64 const RH   // relative humidity value (0.0-1.0)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       Name change to signify derivation and temperatures were used
        //                      with 0C as minimum; LKL January 2008
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Vapor Density in air as a
        // function of dry bulb temperature, and Relative Humidity.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals,

        return RH / (461.52 * (Tdb + DataGlobalConstants::KelvinConv)) *
               std::exp(23.7093 - 4111.0 / ((Tdb + DataGlobalConstants::KelvinConv) - 35.45)); // Vapor density in air
    }

    inline Real64 PsyRhovFnTdbWPb(Real64 const Tdb, // dry-bulb temperature {C}
                                  Real64 const dW,  // humidity ratio
                                  Real64 const PB   // Barometric Pressure {Pascals}
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Vapor Density in air as a
        // function of dry bulb temperature, Humidity Ratio, and Barometric Pressure.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals,

        Real64 const W(max(dW, 1.0e-5)); // humidity ratio
        return W * PB / (461.52 * (Tdb + DataGlobalConstants::KelvinConv) * (W + 0.62198));
    }

    inline Real64 PsyRhovFnTdbWPb_fast(Real64 const Tdb, // dry-bulb temperature {C}
                                       Real64 const dW,  // humidity ratio
                                       Real64 const PB   // Barometric Pressure {Pascals}
    )
    {
        // Faster version with humidity ratio already adjusted
        assert(dW >= 1.0e-5);
        return dW * PB / (461.52 * (Tdb + DataGlobalConstants::KelvinConv) * (dW + 0.62198));
    }

#ifdef EP_psych_errors
    void PsyRhFnTdbRhovLBnd0C_error(EnergyPlusData &state,
                                    Real64 const Tdb,                 // dry-bulb temperature {C}
                                    Real64 const Rhovapor,            // vapor density in air {kg/m3}
                                    Real64 const RHValue,             // relative humidity value (0.0-1.0)
                                    std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64
    PsyRhFnTdbRhovLBnd0C([[maybe_unused]] EnergyPlusData &state,
                         Real64 const Tdb,                                       // dry-bulb temperature {C}
                         Real64 const Rhovapor,                                  // vapor density in air {kg/m3}
                         [[maybe_unused]] std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       Name change to signify derivation and temperatures were used
        //                      with 0C as minimum; LKL January 2008
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Relative Humidity in air as a
        // function of dry bulb temperature and Vapor Density.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals,

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyRhFnTdbRhovLBnd0C);
#endif

        Real64 const RHValue(Rhovapor > 0.0 ? Rhovapor * 461.52 * (Tdb + DataGlobalConstants::KelvinConv) *
                                                  std::exp(-23.7093 + 4111.0 / ((Tdb + DataGlobalConstants::KelvinConv) - 35.45))
                                            : 0.0);

        if ((RHValue < 0.0) || (RHValue > 1.0)) {
#ifdef EP_psych_errors
            if ((RHValue < -0.05) || (RHValue > 1.01)) {
                PsyRhFnTdbRhovLBnd0C_error(state, Tdb, Rhovapor, RHValue, CalledFrom);
            }
#endif
            return min(max(RHValue, 0.01), 1.0);
        } else {
            return RHValue;
        }
    }

#ifdef EP_cache_PsyTwbFnTdbWPb

    Real64 PsyTwbFnTdbWPb(EnergyPlusData &state,
                          Real64 const Tdb,                      // dry-bulb temperature {C}
                          Real64 const W,                        // humidity ratio
                          Real64 const Pb,                       // barometric pressure {Pascals}
                          std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

    Real64 PsyTwbFnTdbWPb_raw(EnergyPlusData &state,
                              Real64 const TDB,                      // dry-bulb temperature {C}
                              Real64 const dW,                       // humidity ratio
                              Real64 const Patm,                     // barometric pressure {Pascals}
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

#else

    Real64 PsyTwbFnTdbWPb(EnergyPlusData &state,
                          Real64 const TDB,                      // dry-bulb temperature {C}
                          Real64 const dW,                       // humidity ratio
                          Real64 const Patm,                     // barometric pressure {Pascals}
                          std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

#endif

#ifdef EP_psych_errors
    void PsyVFnTdbWPb_error(EnergyPlusData &state,
                            Real64 const TDB,                 // dry-bulb temperature {C}
                            Real64 const w,                   // humidity ratio
                            Real64 const PB,                  // barometric pressure {Pascals}
                            Real64 const V,                   // specific volume {m3/kg}
                            std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyVFnTdbWPb([[maybe_unused]] EnergyPlusData &state,
                               Real64 const TDB,                                       // dry-bulb temperature {C}
                               Real64 const dW,                                        // humidity ratio
                               Real64 const PB,                                        // barometric pressure {Pascals}
                               [[maybe_unused]] std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the specific volume from dry-bulb temperature,
        // humidity ratio and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 28

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyVFnTdbWPb);
#endif

        Real64 const w(max(dW, 1.0e-5));                                           // humidity ratio
        Real64 const V(1.59473e2 * (1.0 + 1.6078 * w) * (1.8 * TDB + 492.0) / PB); // specific volume {m3/kg}

        // Validity test
        if (V < 0.0) {
#ifdef EP_psych_errors
            if (V <= -0.01) PsyVFnTdbWPb_error(state, TDB, w, PB, V, CalledFrom);
#endif
            return 0.83; // Fix Was inside the ifdef
        } else {
            return V;
        }
    }

#ifdef EP_psych_errors
    void PsyWFnTdbH_error(EnergyPlusData &state,
                          Real64 const TDB,                 // dry-bulb temperature {C}
                          Real64 const H,                   // enthalpy {J/kg}
                          Real64 const W,                   // humidity ratio
                          std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyWFnTdbH([[maybe_unused]] EnergyPlusData &state,
                             Real64 const TDB,                                        // dry-bulb temperature {C}
                             Real64 const H,                                          // enthalpy {J/kg}
                             [[maybe_unused]] std::string_view const CalledFrom = "", // routine this function was called from (error messages)
                             [[maybe_unused]] bool const SuppressWarnings = false     // if calling function is calculating an intermediate state
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the humidity ratio from dry-bulb temperature
        // and enthalpy.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyWFnTdbH);
#endif

        Real64 const W((H - 1.00484e3 * TDB) / (2.50094e6 + 1.85895e3 * TDB)); // humidity ratio

        // Validity test
        if (W < 0.0) {
#ifdef EP_psych_errors
            if ((W <= -0.0001) && (!SuppressWarnings)) PsyWFnTdbH_error(state, TDB, H, W, CalledFrom);
#endif
            return 1.0e-5;
        } else {
            return W;
        }
    }

#ifdef EP_cache_PsyPsatFnTemp

    Real64 PsyPsatFnTemp_raw(EnergyPlusData &state,
                             Real64 const T,                        // dry-bulb temperature {C}
                             std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

    // we are disabling these warnings on Windows because the cache value lookups are using 64bit integers,
    // but the () and [] operator overloads for Array1D (which stores the cache) only uses 32bit lookups
    // this seems ... very bad. This problem will be fixed when we get rid of Array1D
    // at which time this warning disable should be removed.
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4244)
#endif

    inline Real64 PsyPsatFnTemp(EnergyPlusData &state,
                                Real64 const T,                        // dry-bulb temperature {C}
                                std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Provide a "cache" of results for the given argument (T) and pressure (Pascal) output result.

        // METHODOLOGY EMPLOYED:
        // Use grid shifting and masking to provide hash into the cache. Use Equivalence to
        // make Fortran ignore "types".

        // FUNCTION PARAMETER DEFINITIONS:
        //  integer(i64), parameter :: Grid_Mask=NOT(ISHFT(1_i64, Grid_Shift)-1)
        Int64 const Grid_Shift(28);                         // Tuned This is a hot spot
        assert(Grid_Shift == 64 - 12 - psatprecision_bits); // Force Grid_Shift updates when precision bits changes

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyPsatFnTemp_cache);
#endif

        Int64 const Tdb_tag(bit_shift(bit_transfer(T, Grid_Shift), -Grid_Shift)); // Note that 2nd arg to TRANSFER is not used: Only type matters
        //        Int64 const hash( bit::bit_and( Tdb_tag, psatcache_mask ) ); //Tuned Replaced by below
        Int64 const hash(Tdb_tag & psatcache_mask);
        auto &cPsat(state.dataPsychCache->cached_Psat(hash));

        if (cPsat.iTdb != Tdb_tag) {
            cPsat.iTdb = Tdb_tag;
            Real64 Tdb_tag_r;
            Tdb_tag_r = bit_transfer(bit_shift(Tdb_tag, Grid_Shift), Tdb_tag_r);
            cPsat.Psat = PsyPsatFnTemp_raw(state, Tdb_tag_r, CalledFrom);
        }

        return cPsat.Psat; // saturation pressure {Pascals}
    }

#else

    Real64 PsyPsatFnTemp(EnergyPlusData &state,
                         Real64 const T,                        // dry-bulb temperature {C}
                         std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

#endif

#ifdef EP_cache_PsyTsatFnHPb
    Real64 PsyTsatFnHPb_raw(EnergyPlusData &state,
                            Real64 const H,                        // enthalpy {J/kg}
                            Real64 const PB,                       // barometric pressure {Pascals}
                            std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );
    inline Real64 PsyTsatFnHPb(EnergyPlusData &state,
                               Real64 const H,
                               Real64 const Pb,                       // barometric pressure {Pascals}
                               std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {

        Real64 Tsat_result; // result=> Sat-Temp {C}

        Int64 const Grid_Shift(64 - 12 - tsat_hbp_precision_bits);

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Int64 H_tag;
        Int64 Pb_tag;
        Int64 hash;

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyTwbFnTdbWPb_cache);
#endif

        H_tag = bit_transfer(H, H_tag);
        H_tag = bit_shift(H_tag, -Grid_Shift);
        Pb_tag = bit_transfer(Pb, Pb_tag);
        Pb_tag = bit_shift(Pb_tag, -Grid_Shift);
        hash = bit_and(bit_xor(H_tag, Pb_tag), Int64(tsat_hbp_cache_size - 1));
        auto &cached_Tsat_HPb = state.dataPsychCache->cached_Tsat_HPb;
        if (cached_Tsat_HPb(hash).iH != H_tag || cached_Tsat_HPb(hash).iPb != Pb_tag) {
            cached_Tsat_HPb(hash).iH = H_tag;
            cached_Tsat_HPb(hash).iPb = Pb_tag;
            cached_Tsat_HPb(hash).Tsat = PsyTsatFnHPb_raw(state, H, Pb, CalledFrom);
        }

        Tsat_result = cached_Tsat_HPb(hash).Tsat;

        return Tsat_result;
    }

#else

    Real64 PsyTsatFnHPb(EnergyPlusData &state,
                        Real64 const H,                        // enthalpy {J/kg}
                        Real64 const PB,                       // barometric pressure {Pascals}
                        std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

#endif

    inline Real64 PsyRhovFnTdbRh(EnergyPlusData &state,
                                 Real64 const Tdb,                      // dry-bulb temperature {C}
                                 Real64 const RH,                       // relative humidity value (0.0-1.0)
                                 std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       Change temperature range applied (determine pws); Aug 2007; LKL
        //                      Function is continuous over temperature spectrum
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Vapor Density in air as a
        // function of dry bulb temperature, and Relative Humidity.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals, ??
        // Used values from Table 2, HOF 2005, Chapter 6, to verify that these values match (at saturation)
        // values from PsyRhFnTdbWPb

        return (PsyPsatFnTemp(state, Tdb, CalledFrom) * RH) / (461.52 * (Tdb + DataGlobalConstants::KelvinConv)); // Vapor density in air
    }

#ifdef EP_psych_errors
    void PsyRhFnTdbRhov_error(EnergyPlusData &state,
                              Real64 const Tdb,                      // dry-bulb temperature {C}
                              Real64 const Rhovapor,                 // vapor density in air {kg/m3}
                              Real64 const RHValue,                  // relative humidity
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyRhFnTdbRhov(EnergyPlusData &state,
                                 Real64 const Tdb,                                       // dry-bulb temperature {C}
                                 Real64 const Rhovapor,                                  // vapor density in air {kg/m3}
                                 [[maybe_unused]] std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       Change temperature range applied (determine pws); Aug 2007; LKL
        //                      Function is continuous over temperature spectrum
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Relative Humidity in air as a
        // function of dry bulb temperature and Vapor Density.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals,
        // Used values from Table 2, HOF 2005, Chapter 6, to verify that these values match (at saturation)
        // values from PsyRhFnTdbWPb

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyRhFnTdbRhov");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyRhFnTdbRhov);
#endif

        Real64 const RHValue(Rhovapor > 0.0 ? Rhovapor * 461.52 * (Tdb + DataGlobalConstants::KelvinConv) / PsyPsatFnTemp(state, Tdb, RoutineName)
                                            : 0.0);

        if ((RHValue < 0.0) || (RHValue > 1.0)) {
#ifdef EP_psych_errors
            if ((RHValue < -0.05) || (RHValue > 1.01)) {
                PsyRhFnTdbRhov_error(state, Tdb, Rhovapor, RHValue, CalledFrom);
            }
#endif
            return min(max(RHValue, 0.01), 1.0);
        } else {
            return RHValue;
        }
    }

#ifdef EP_psych_errors
    void PsyRhFnTdbWPb_error(EnergyPlusData &state,
                             Real64 const TDB,                 // dry-bulb temperature {C}
                             Real64 const W,                   // humidity ratio
                             Real64 const RHValue,             // relative humidity (0.0-1.0)
                             std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyRhFnTdbWPb(EnergyPlusData &state,
                                Real64 const TDB,                      // dry-bulb temperature {C}
                                Real64 const dW,                       // humidity ratio
                                Real64 const PB,                       // barometric pressure {Pascals}
                                std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   Nov 1988
        //       MODIFIED       Aug 1989, Michael J. Witte
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the relative humidity value (0.0-1.0) as a result of
        // dry-bulb temperature, humidity ratio and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK FUNDAMENTALS 1985, P6.12, EQN 10,21,23

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyRhFnTdbWPb");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyRhFnTdbWPb);
#endif

        Real64 const PWS(PsyPsatFnTemp(state, TDB, (CalledFrom.empty() ? RoutineName : CalledFrom))); // Pressure -- saturated for pure water

        // Find Degree Of Saturation
        Real64 const W(max(dW, 1.0e-5));                  // humidity ratio
        Real64 const U(W / (0.62198 * PWS / (PB - PWS))); // Degree of Saturation

        // Calculate The Relative Humidity
        Real64 const RHValue(U / (1.0 - (1.0 - U) * (PWS / PB)));

        // Validity test
        if ((RHValue < 0.0) || (RHValue > 1.0)) {
#ifdef EP_psych_errors
            if ((RHValue < -0.05) || (RHValue > 1.01)) {
                PsyRhFnTdbWPb_error(state, TDB, W, RHValue, CalledFrom);
            }
#endif
            return min(max(RHValue, 0.01), 1.0);
        } else {
            return RHValue;
        }
    }

#ifdef EP_psych_errors
    void PsyWFnTdpPb_error(EnergyPlusData &state,
                           Real64 const TDP,                 // dew-point temperature {C}
                           Real64 const PB,                  // barometric pressure {Pascals}
                           Real64 const W,                   // humidity ratio
                           Real64 const DeltaT,              // Reduced temperature difference of dew point
                           std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyWFnTdpPb(EnergyPlusData &state,
                              Real64 const TDP,                      // dew-point temperature {C}
                              Real64 const PB,                       // barometric pressure {Pascals}
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the humidity ratio from dew-point temperature
        // and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyWFnTdpPb");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyWFnTdpPb);
#endif

        Real64 const PDEW(
            PsyPsatFnTemp(state, TDP, (CalledFrom.empty() ? RoutineName : CalledFrom))); // saturation pressure at dew-point temperature {Pascals}
        Real64 const W(PDEW * 0.62198 / (PB - PDEW));                                    // humidity ratio

        // Validity test
        if (W < 0.0) {
            Real64 DeltaT = 0.0;
            Real64 PDEW1 = PDEW;
            while (PDEW1 >= PB) {
                DeltaT++;
                PDEW1 = PsyPsatFnTemp(state,
                                      TDP - DeltaT,
                                      (CalledFrom.empty() ? RoutineName : CalledFrom)); // saturation pressure at dew-point temperature {Pascals}
            }
            Real64 W1 = PDEW1 * 0.62198 / (PB - PDEW1);
#ifdef EP_psych_errors
            if (W <= -0.0001) {
                PsyWFnTdpPb_error(state, TDP, PB, W1, DeltaT, CalledFrom);
            }
#endif
            return W1;
        } else {
            return W;
        }
    }

#ifdef EP_psych_errors
    void PsyWFnTdbRhPb_error(EnergyPlusData &state,
                             Real64 const TDB,                 // dry-bulb temperature {C}
                             Real64 const RH,                  // relative humidity value (0.0-1.0)
                             Real64 const PB,                  // barometric pressure {Pascals}
                             Real64 const W,                   // humidity ratio
                             std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyWFnTdbRhPb(EnergyPlusData &state,
                                Real64 const TDB,                      // dry-bulb temperature {C}
                                Real64 const RH,                       // relative humidity value (0.0-1.0)
                                Real64 const PB,                       // barometric pressure {Pascals}
                                std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the humidity ratio from dry-bulb temperature,
        // relative humidty (value) and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyWFnTdbRhPb");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyWFnTdbRhPb);
#endif

        Real64 const PDEW(RH *
                          PsyPsatFnTemp(state, TDB, (CalledFrom.empty() ? RoutineName : CalledFrom))); // Pressure at dew-point temperature {Pascals}

        // Numeric error check when the temperature and RH values cause Pdew to equal or exceed
        // barometric pressure which is physically impossible. An approach limit of 1000 pascals
        // was chosen to keep the numerics stable as the denominator approaches 0.
        Real64 const W(PDEW * 0.62198 / max(PB - PDEW, 1000.0)); // humidity ratio
        // THIS EQUATION IN SI UNIT IS FROM ASHRAE HANDBOOK OF FUNDAMENTALS PAGE 99  EQUATION 22

        // Validity test
        if (W < 1.0e-5) {
#ifdef EP_psych_errors
            if (W <= -0.0001) PsyWFnTdbRhPb_error(state, TDB, RH, PB, W, CalledFrom);
#endif
            return 1.0e-5;
        } else {
            return W;
        }
    }

#ifdef EP_psych_errors

    void PsyWFnTdbTwbPb_temperature_error(EnergyPlusData &state,
                                          Real64 const TDB,                 // dry-bulb temperature {C}
                                          Real64 const TWB,                 // wet-bulb temperature {C}
                                          Real64 const PB,                  // barometric pressure {Pascals}
                                          std::string_view const CalledFrom // routine this function was called from (error messages)
    );

    void PsyWFnTdbTwbPb_humidity_error(EnergyPlusData &state,
                                       Real64 const TDB,                 // dry-bulb temperature {C}
                                       Real64 const TWB,                 // wet-bulb temperature {C}
                                       Real64 const PB,                  // barometric pressure {Pascals}
                                       Real64 const W,                   // humidity ratio
                                       std::string_view const CalledFrom // routine this function was called from (error messages)
    );

#endif

    inline Real64 PsyWFnTdbTwbPb(EnergyPlusData &state,
                                 Real64 const TDB,                      // dry-bulb temperature {C}
                                 Real64 const TWBin,                    // wet-bulb temperature {C}
                                 Real64 const PB,                       // barometric pressure {Pascals}
                                 std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the humidity ratio from dry-bulb temperature,
        // wet-bulb temperature and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQ 22,35

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyWFnTdbTwbPb");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyWFnTdbTwbPb);
#endif

        Real64 TWB(TWBin); // test wet-bulb temperature

        // Validity check
        if (TWB > TDB) {
#ifdef EP_psych_errors
            if (TWB > TDB + 0.01) PsyWFnTdbTwbPb_temperature_error(state, TDB, TWB, PB, CalledFrom);
#endif
            TWB = TDB;
        }

        // Calculation
        Real64 const PWET(PsyPsatFnTemp(state, TWB, (CalledFrom.empty() ? RoutineName : CalledFrom))); // Pressure at wet-bulb temperature {Pascals}
        Real64 const WET(0.62198 * PWET / (PB - PWET));                                                // Humidity ratio at wet-bulb temperature
        Real64 const W(((2501.0 - 2.381 * TWB) * WET - (TDB - TWB)) / (2501.0 + 1.805 * TDB - 4.186 * TWB)); // humidity ratio

        // Validity check
        if (W < 0.0) {
#ifdef EP_psych_errors
            PsyWFnTdbTwbPb_humidity_error(state, TDB, TWB, PB, W, CalledFrom);
#endif
            return PsyWFnTdbRhPb(state, TDB, 0.0001, PB, CalledFrom);
        } else {
            return W;
        }
    }

    inline Real64 PsyHFnTdbRhPb(EnergyPlusData &state,
                                Real64 const TDB,                      // dry-bulb temperature {C}
                                Real64 const RH,                       // relative humidity value (0.0 - 1.0)
                                Real64 const PB,                       // barometric pressure (N/M**2) {Pascals}
                                std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         J. C. VanderZee
        //       DATE WRITTEN   Feb. 1994
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides air enthalpy from temperature and relative humidity.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32
        //   by using functions PsyWFnTdbRhPb and PsyHFnTdbW

        return PsyHFnTdbW(TDB, max(PsyWFnTdbRhPb(state, TDB, RH, PB, CalledFrom), 1.0e-5)); // enthalpy {J/kg}
    }

#ifdef EP_cache_PsyTsatFnPb

    Real64 PsyTsatFnPb_raw(EnergyPlusData &state,
                           Real64 const Press,                    // barometric pressure {Pascals}
                           std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

    inline Real64 PsyTsatFnPb(EnergyPlusData &state,
                              Real64 const Press,                    // barometric pressure {Pascals}
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {

        Int64 const Grid_Shift(28);                         // Tuned This is a hot spot
        assert(Grid_Shift == 64 - 12 - tsatprecision_bits); // Force Grid_Shift updates when precision bits changes
        Int64 const Pb_tag(bit_shift(bit_transfer(Press, Grid_Shift), -Grid_Shift));

        Int64 const hash(Pb_tag & tsatcache_mask);
        auto &cTsat(state.dataPsychCache->cached_Tsat(hash));
        if (cTsat.iPb != Pb_tag) {
            cTsat.iPb = Pb_tag;
            cTsat.Tsat = PsyTsatFnPb_raw(state, Press, CalledFrom);
        }

        return cTsat.Tsat; // saturation temperature
    }

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#else
    Real64 PsyTsatFnPb(EnergyPlusData &state,
                       Real64 const Press,                    // barometric pressure {Pascals}
                       std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyTdpFnWPb(EnergyPlusData &state,
                              Real64 const W,                        // humidity ratio
                              Real64 const PB,                       // barometric pressure (N/M**2) {Pascals}
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the dew-point temperature {C} from humidity ratio and pressure.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P.99, EQN 22

        Real64 const W0(max(W, 1.0e-5));             // limited humidity ratio
        Real64 const PDEW(PB * W0 / (0.62198 + W0)); // pressure at dew point temperature
        return PsyTsatFnPb(state, PDEW, CalledFrom);
    }

#ifdef EP_psych_errors
    void PsyTdpFnTdbTwbPb_error(EnergyPlusData &state,
                                Real64 const TDB,                 // dry-bulb temperature {C}
                                Real64 const TWB,                 // wet-bulb temperature {C}
                                Real64 const PB,                  // barometric pressure (N/M**2) {Pascals}
                                Real64 const W,                   // humidity ratio
                                Real64 const TDP,                 // dew-point temperature {C}
                                std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyTdpFnTdbTwbPb(EnergyPlusData &state,
                                   Real64 const TDB,                      // dry-bulb temperature {C}
                                   Real64 const TWB,                      // wet-bulb temperature {C}
                                   Real64 const PB,                       // barometric pressure (N/M**2) {Pascals}
                                   std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the dew-point temperature {C} from dry-bulb, wet-bulb and pressure.

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyTdpFnTdbTwbPb);
#endif

        Real64 const W(max(PsyWFnTdbTwbPb(state, TDB, TWB, PB, CalledFrom), 1.0e-5));
        Real64 const TDP(PsyTdpFnWPb(state, W, PB, CalledFrom));

        if (TDP > TWB) {
#ifdef EP_psych_errors
            if (TDP > TWB + 0.1) PsyTdpFnTdbTwbPb_error(state, TDB, TWB, PB, W, TDP, CalledFrom);
#endif
            return TWB;
        } else {
            return TDP;
        }
    }

    inline Real64 F6(Real64 const X, Real64 const A0, Real64 const A1, Real64 const A2, Real64 const A3, Real64 const A4, Real64 const A5)
    {
        return A0 + X * (A1 + X * (A2 + X * (A3 + X * (A4 + X * A5))));
    }

    inline Real64
    F7(Real64 const X, Real64 const A0, Real64 const A1, Real64 const A2, Real64 const A3, Real64 const A4, Real64 const A5, Real64 const A6)
    {
        return (A0 + X * (A1 + X * (A2 + X * (A3 + X * (A4 + X * (A5 + X * A6)))))) / 1.0E10;
    }

    inline Real64 CPCW([[maybe_unused]] Real64 const Temperature // unused1208
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         RUSSELL D. TAYLOR
        //       DATE WRITTEN   April 1992

        // PURPOSE OF THIS FUNCTION:
        // This function provides the specific heat of chilled water. CPCW (J/Kg/k)

        return 4180.0;
    }

    inline Real64 CPHW([[maybe_unused]] Real64 const Temperature // unused1208
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         RUSSELL D. TAYLOR
        //       DATE WRITTEN   April 1992

        // PURPOSE OF THIS FUNCTION:
        // This function provides the specific heat of hot water. CPHW (J/Kg/k)

        return 4180.0;
    }

    inline Real64 RhoH2O(Real64 const TB // Dry bulb temperature. {C}
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         SIGSTEINN P. GRETARSSON
        //       DATE WRITTEN   April 1992

        // PURPOSE OF THIS FUNCTION:
        // This function provides the density of water at a specific temperature.

        // METHODOLOGY EMPLOYED:
        //     Density of water [kg/m3]
        //     (RANGE: KelvinConv - 423.15 DEG. K) (convert to C first)

        return 1000.1207 + 8.3215874e-04 * TB - 4.929976e-03 * pow_2(TB) + 8.4791863e-06 * pow_3(TB);
    }

    inline Real64 PsyDeltaHSenFnTdb2Tdb1W(Real64 const TDB2, // dry-bulb temperature at state 1 {C}
                                          Real64 const TDB1, // dry-bulb temperature at state 2 {C}
                                          Real64 const W     // humidity ratio (at zone air node or Wmin)
    )
    {
        // When called for zone equipment flow entering a zone (from CalcZoneSensibleLatentOutput or CalcZoneSensibleOutput):
        // returns sensible enthalpy difference between equipment supply air (TDB2) and zone air (TDB1) evaluated
        // using the zone air node humidity ratio. This enthalpy difference multiplied by supply
        // air mass flow rate yields the sensible heat transfer rate in Watts.
        // positive value is heating, negative value is cooling

        // When called across a component (from PsyDeltaHSenFnTdb2W2Tdb1W1 by CalcComponentSensibleLatentOutput):
        // returns sensible enthalpy difference between state 1 (TDB1) and state 2 (TDB2) using the minimum
        // humidity ratio from states 1 and 2. This enthalpy difference multiplied by supply air mass flow
        // rate yields the sensible heat transfer rate in Watts.
        // positive value is heating, negative value is cooling

        // the following two functions for calculating enthalpy difference are equivalent:
        // PsyDeltaHSenFnTdb2Tdb1W() = PsyHFnTdbW(TDB2, W) - PsyHFnTdbW(TDB1, W)
        // PsyDeltaHSenFnTdb2Tdb1W() function was derived by simplifying the expression above
        // The constant coefficients come from the equation for moist air enthalpy, PsyHFnTdbW()

        return (1.00484e3 + max(1.0e-5, W) * 1.85895e3) * (TDB2 - TDB1);
    }

    inline Real64 PsyDeltaHSenFnTdb2W2Tdb1W1(Real64 const TDB2, // dry-bulb temperature at state 2 {C}
                                             Real64 const W2,   // humidity ratio at state 2
                                             Real64 const TDB1, // dry-bulb temperature at state 1 {C}
                                             Real64 const W1    // humidity ratio at state 1
    )
    {
        // returns sensible enthalpy difference of moist air going from state 1 to state 2 (e.g across coils)
        // using the minimum humidity ratio state points 1 and 2. This enthalpy difference multiplied by
        // supply air mass flow rate yields sensible heat transfer rate across coils in Watts
        // positive value is heating, negative value is cooling

        // the following two functions for calculating enthalpy difference are equivalent:
        // PsyDeltaHSenFnTdb2W2Tdb1W1() = PsyHFnTdbW(TDB2, min(W1, W2)) - PsyHFnTdbW(TDB1, min(W1,W2))
        // PsyDeltaHSenFnTdb2W2Tdb1W1() function was derived by simplifying the above expression
        // The constant coefficients came from the equation for moist air enthalpy, PsyHFnTdbW()

        Real64 const Wmin = min(W1, W2);
        return PsyDeltaHSenFnTdb2Tdb1W(TDB2, TDB1, Wmin);
    }

} // namespace Psychrometrics

struct PsychrometricsData : BaseGlobalStruct
{
    Real64 iconvTol = 0.0001;
    Real64 last_Patm = -99999.0;  // barometric pressure {Pascals}  (last)
    Real64 last_tBoil = -99999.0; // Boiling temperature of water at given pressure (last)
    Real64 Press_Save = -99999.0;
    Real64 tSat_Save = -99999.0;
    Array1D_int iPsyErrIndex = Array1D_int(EnergyPlus::NumPsychMonitors, 0); // Number of times error occurred
    std::string String;
    bool ReportErrors = true;

    void clear_state() override
    {
        iPsyErrIndex = Array1D_int(EnergyPlus::NumPsychMonitors, 0);
        iconvTol = 0.0001;
        last_Patm = -99999.0;  // barometric pressure {Pascals}  (last)
        last_tBoil = -99999.0; // Boiling temperature of water at given pressure (last)
        Press_Save = -99999.0;
        tSat_Save = -99999.0;
        String = "";
        ReportErrors = true;
    }
};

} // namespace EnergyPlus

#endif
