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

// C++ Headers
#include <cassert>
#include <cstddef>
#include <functional>
#include <limits>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace FluidProperties {

    // MODULE INFORMATION:
    //       AUTHOR         Mike Turner
    //       DATE WRITTEN   10 December 99
    //       MODIFIED       Rick Strand (April 2000, May 2000)
    //                      Simon Rees  (May, June 2002)
    //                      Rick Strand (June 2004)
    //                      Linda Lawrie (March 2008)
    //       RE-ENGINEERED  Rick Strand (April 2000, May 2000)

    // PURPOSE OF THIS MODULE:
    // This module contains subroutines which determine and return properties
    // of materials including enthalpy, quality, specific heat, and density.
    // The module uses InputProcessor to read the material type and the
    // associated charts from IN.IDF.  The module is only as powerful as the
    // amount of data loaded into this file.

    // METHODOLOGY EMPLOYED:
    // The module will first check if the current refrigerant has been read
    // in yet.  If not, it will get the data from IN.IDF and "store" it into
    // a set of variables.  Any future iterations with that refrigerant will
    // simply retrieve the data from storage instead of reading from the .IDF
    // file again.  After the data is made available, the module uses input
    // temperatures, pressures, and either quality or enthalpy to locate the
    // state point and choose the proper routine.  Finally, it performs a
    // double interpolation between temperatures and pressures or qualities
    // which surround the point on a chart specified by the input conditions.
    // The program is designed to work on either side of or under the vapor
    // dome.  This data can be added as needed.
    // Where properties are invalid at particular pressure/temperature points
    // in the data input file, zeros have to be inserted. This is necessary
    // as the data structures are rectangular. The zero values are used to detect
    // bounds of the data and issue appropriate warnings.
    // Properties of liquids (e.g. water) can be specified as glycol properties by
    // supplying the same data for concentrations of 0.0 and 1.0 only.
    // Temperature data has to be supplied in ascending order only.

    // Data
    // MODULE PARAMETER DEFINITIONS

    // DERIVED TYPE DEFINITIONS

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // MODULE VARIABLE DECLARATIONS

    // ACCESSIBLE SPECIFICATIONS OF MODULE SUBROUTINES OR FUNCTONS:

    // Object Data

#ifdef EP_cache_GlycolSpecificHeat
    std::array<cached_tsh, t_sh_cache_size> cached_t_sh;
#endif
    // Data Initializer Forward Declarations
    // See GetFluidPropertiesData "SUBROUTINE LOCAL DATA" for actual data.

    // MODULE SUBROUTINES:

    // Functions

    void InitializeGlycRoutines()
    {
#ifdef EP_cache_GlycolSpecificHeat
        cached_t_sh.fill({});
#endif
    }

    void GetFluidPropertiesData(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   April 2000
        //       MODIFIED       May 2002 Simon Rees (Added saturated pressure data retreaval)
        //                      June 2004 Rick Strand (Added glycol defaults and modified glycol data structure)
        //                      August 2011 Linda Lawrie (Added steam as default refrigerant)
        //                      August 2012 Linda Lawrie (more error checks on data input)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // The purpose of this subroutine is to read in all of the fluid
        // property data contained in the user input file.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.  Derived type portions are
        // allocated as necessary as the data is read into the program.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace std::placeholders; // For use with 'std::bind' in Array initializers

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const PressToler(1.0); // Some reasonable value for comparisons
        static constexpr std::string_view RoutineName("GetFluidPropertiesData: ");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string Alphas;             // Reads string value from input file
        Array1D_string cAlphaFieldNames;   // field names for alpha fields
        Array1D_string cNumericFieldNames; // field names for numeric fields
        int Loop;                          // DO loop counter (various uses)
        int NumAlphas;                     // States which alpha value to read from a "Number" line
        Array1D<Real64> Numbers;           // brings in data from IP
        Array1D_bool lAlphaFieldBlanks;    // logical for blank alpha fields
        Array1D_bool lNumericFieldBlanks;  // logical for blank numeric fields
        int NumNumbers;                    // States which number value to read from a "Numbers" line
        int MaxAlphas;                     // maximum number of alphas
        int MaxNumbers;                    // maximum number of numbers
        int Status;                        // Either 1 "object found" or -1 "not found" (also used as temp)
        int InData;
        int TempLoop;
        int NumOfFluidTempArrays;
        int NumOfSatFluidPropArrays;
        int NumOfSHFluidPropArrays;
        int NumOfGlyFluidPropArrays;
        std::string TempsName;
        bool FirstSHMatch;
        int NumOfPressPts;
        int NumOfConcPts;
        bool ErrorsFound(false);
        int Index;
        int NumOfGlyConcs;
        bool GlycolFound;
        int NumOfOptionalInput;
        std::string CurrentModuleObject; // for ease in renaming.
        Real64 pTemp;
        int iTemp;
        int j;
        int FluidNum;

        // SUBROUTINE LOCAL DATA:

        // Note two methods of Array initialization in use:
        // - Fixed list of values.  The second parameter is an initializer list (brace
        //   delimited list of numbers).
        // - Initializer function.  The second parameter is the function name.
        //   In several cases we need to pass water data to the initializer, but an
        //   Array initializer only takes one argument.  std::bind is used to convert the
        //   actual initializer into a function of one argument.

        // For default "glycol" fluids of Water, Ethylene Glycol, and Propylene Glycol

        constexpr std::array<Real64, DefaultNumGlyTemps> DefaultGlycolTemps = {
            -35.0, -30.0, -25.0, -20.0, -15.0, -10.0, -5.0, 0.0,  5.0,  10.0, 15.0,  20.0,  25.0,  30.0,  35.0,  40.0, 45.0,
            50.0,  55.0,  60.0,  65.0,  70.0,  75.0,  80.0, 85.0, 90.0, 95.0, 100.0, 105.0, 110.0, 115.0, 120.0, 125.0}; // 33 total temperature
        // points

        constexpr std::array<Real64, DefaultNumGlyConcs> DefaultGlycolConcs = {
            0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9}; // 10 total concentration points

        constexpr std::array<Real64, DefaultNumGlyTemps> DefaultWaterCpData = {0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    4217.0, 4198.0,
                                                                               4191.0, 4185.0, 4181.0, 4179.0, 4180.0, 4180.0, 4180.0, 4180.0, 4181.0,
                                                                               4183.0, 4185.0, 4188.0, 4192.0, 4196.0, 4200.0, 4203.0, 4208.0, 4213.0,
                                                                               4218.0, 4223.0, 4228.0, 4233.0, 4238.0, 4243.0}; // in J/kg-K

        constexpr std::array<Real64, DefaultNumGlyTemps> DefaultWaterViscData = {
            0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    1.7912e-3, 1.5183e-3, 1.306e-3,  1.1376e-3,
            1.0016e-3, 0.8901e-3, 0.7974e-3, 0.7193e-3, 0.653e-3,  0.5961e-3, 0.5468e-3, 0.504e-3,  0.4664e-3, 0.4332e-3, 0.4039e-3,
            0.3777e-3, 0.3543e-3, 0.3333e-3, 0.3144e-3, 0.2973e-3, 0.2817e-3, 0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3}; // in Pa-s

        constexpr std::array<Real64, DefaultNumGlyTemps> DefaultWaterRhoData = {
            0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   999.8, 999.9, 999.7, 999.1, 998.2, 997.0, 995.6, 994.0, 992.2, 990.2,
            988.0, 985.7, 983.2, 980.5, 977.7, 974.8, 971.8, 968.6, 965.3, 961.9, 958.3, 0.0,   0.0,   0.0,   0.0,   0.0}; // in kg/m3

        constexpr std::array<Real64, DefaultNumGlyTemps> DefaultWaterCondData = {
            0.0,    0.0,    0.0,    0.0,   0.0,    0.0,    0.0,  0.561,  0.5705, 0.58,   0.5893, 0.5984, 0.6072, 0.6155, 0.6233, 0.6306, 0.6373,
            0.6436, 0.6492, 0.6543, 0.659, 0.6631, 0.6668, 0.67, 0.6728, 0.6753, 0.6773, 0.6791, 0.0,    0.0,    0.0,    0.0,    0.0}; // in W/mK

        // Ethylene Glycol Data: Specific Heat in J/(kg-k)

        constexpr std::array<std::array<Real64, DefaultNumGlyTemps>, DefaultNumGlyConcs> DefaultEthGlyCpData = {
            {
                {0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    4217.0, 4198.0, 4191.0, 4185.0,
                 4181.0, 4179.0, 4180.0, 4180.0, 4180.0, 4180.0, 4181.0, 4183.0, 4185.0, 4188.0, 4192.0,
                 4196.0, 4200.0, 4203.0, 4208.0, 4213.0, 4218.0, 4223.0, 4228.0, 4233.0, 4238.0, 4243.0}, // DefaultWaterCpData
                {0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    3937.0, 3946.0, 3954.0, 3963.0,
                 3972.0, 3981.0, 3989.0, 3998.0, 4007.0, 4015.0, 4024.0, 4033.0, 4042.0, 4050.0, 4059.0,
                 4068.0, 4077.0, 4085.0, 4094.0, 4103.0, 4112.0, 4120.0, 4129.0, 4138.0, 4147.0, 4155.0}, // Conc=0.1
                {0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    3757.0, 3769.0, 3780.0, 3792.0, 3803.0,
                 3815.0, 3826.0, 3838.0, 3849.0, 3861.0, 3872.0, 3884.0, 3895.0, 3907.0, 3918.0, 3930.0,
                 3941.0, 3953.0, 3964.0, 3976.0, 3987.0, 3999.0, 4010.0, 4022.0, 4033.0, 4045.0, 4056.0}, // Conc=0.2
                {0.0,    0.0,    0.0,    0.0,    0.0,    3560.0, 3574.0, 3589.0, 3603.0, 3617.0, 3631.0,
                 3645.0, 3660.0, 3674.0, 3688.0, 3702.0, 3716.0, 3730.0, 3745.0, 3759.0, 3773.0, 3787.0,
                 3801.0, 3816.0, 3830.0, 3844.0, 3858.0, 3872.0, 3886.0, 3901.0, 3915.0, 3929.0, 3943.0}, // Conc=0.3
                {0.0,    0.0,    0.0,    3334.0, 3351.0, 3367.0, 3384.0, 3401.0, 3418.0, 3435.0, 3451.0,
                 3468.0, 3485.0, 3502.0, 3518.0, 3535.0, 3552.0, 3569.0, 3585.0, 3602.0, 3619.0, 3636.0,
                 3653.0, 3669.0, 3686.0, 3703.0, 3720.0, 3736.0, 3753.0, 3770.0, 3787.0, 3804.0, 3820.0}, // Conc=0.4
                {3068.0, 3088.0, 3107.0, 3126.0, 3145.0, 3165.0, 3184.0, 3203.0, 3223.0, 3242.0, 3261.0,
                 3281.0, 3300.0, 3319.0, 3339.0, 3358.0, 3377.0, 3396.0, 3416.0, 3435.0, 3454.0, 3474.0,
                 3493.0, 3512.0, 3532.0, 3551.0, 3570.0, 3590.0, 3609.0, 3628.0, 3647.0, 3667.0, 3686.0}, // Conc=0.5
                {2844.0, 2866.0, 2888.0, 2909.0, 2931.0, 2953.0, 2975.0, 2997.0, 3018.0, 3040.0, 3062.0,
                 3084.0, 3106.0, 3127.0, 3149.0, 3171.0, 3193.0, 3215.0, 3236.0, 3258.0, 3280.0, 3302.0,
                 3324.0, 3345.0, 3367.0, 3389.0, 3411.0, 3433.0, 3454.0, 3476.0, 3498.0, 3520.0, 3542.0}, // Conc=0.6
                {2612.0, 2636.0, 2660.0, 2685.0, 2709.0, 2733.0, 2757.0, 2782.0, 2806.0, 2830.0, 2854.0,
                 2878.0, 2903.0, 2927.0, 2951.0, 2975.0, 3000.0, 3024.0, 3048.0, 3072.0, 3097.0, 3121.0,
                 3145.0, 3169.0, 3193.0, 3218.0, 3242.0, 3266.0, 3290.0, 3315.0, 3339.0, 3363.0, 3387.0}, // Conc=0.7
                {2370.0, 2397.0, 2423.0, 2450.0, 2477.0, 2503.0, 2530.0, 2556.0, 2583.0, 2610.0, 2636.0,
                 2663.0, 2690.0, 2716.0, 2743.0, 2770.0, 2796.0, 2823.0, 2850.0, 2876.0, 2903.0, 2929.0,
                 2956.0, 2983.0, 3009.0, 3036.0, 3063.0, 3089.0, 3116.0, 3143.0, 3169.0, 3196.0, 3223.0}, // Conc=0.8
                {0.0,    0.0,    2177.0, 2206.0, 2235.0, 2264.0, 2293.0, 2322.0, 2351.0, 2380.0, 2409.0,
                 2438.0, 2467.0, 2496.0, 2525.0, 2554.0, 2583.0, 2612.0, 2641.0, 2670.0, 2699.0, 2728.0,
                 2757.0, 2786.0, 2815.0, 2844.0, 2873.0, 2902.0, 2931.0, 2960.0, 2989.0, 3018.0, 3047.0} // Conc=0.9
            }};

        // Ethylene Glycol Data: Viscosity in mPa-s
        constexpr std::array<std::array<Real64, DefaultNumGlyTemps>, DefaultNumGlyConcs> DefaultEthGlyViscData = {{
            {0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    1.7912e-3, 1.5183e-3, 1.306e-3,  1.1376e-3,
             1.0016e-3, 0.8901e-3, 0.7974e-3, 0.7193e-3, 0.653e-3,  0.5961e-3, 0.5468e-3, 0.504e-3,  0.4664e-3, 0.4332e-3, 0.4039e-3,
             0.3777e-3, 0.3543e-3, 0.3333e-3, 0.3144e-3, 0.2973e-3, 0.2817e-3, 0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3},
            {0.0e+00,  0.0e+00,  0.0e+00, 0.0e+00, 0.0e+00, 0.0e+00, 0.0e+00, 2.08e-03, 1.79e-03, 1.56e-03, 1.37e-03,
             1.21e-03, 1.08e-03, 9.7e-04, 8.8e-04, 8.0e-04, 7.3e-04, 6.7e-04, 6.2e-04,  5.7e-04,  5.3e-04,  5.0e-04,
             4.7e-04,  4.4e-04,  4.1e-04, 3.9e-04, 3.7e-04, 3.5e-04, 3.3e-04, 3.2e-04,  3.0e-04,  2.9e-04,  2.8e-04}, // Conc = 0.1
            {0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00, 3.65e-03, 3.02e-03, 2.54e-03, 2.18e-03, 1.89e-03,
             1.65e-03, 1.46e-03, 1.30e-03, 1.17e-03, 1.06e-03, 9.6e-04, 8.8e-04,  8.1e-04,  7.4e-04,  6.9e-04,  6.4e-04,
             5.9e-04,  5.5e-04,  5.2e-04,  4.9e-04,  4.6e-04,  4.3e-04, 4.0e-04,  3.8e-04,  3.6e-04,  3.4e-04,  3.3e-04}, // Conc = 0.2
            {0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  6.19e-03, 5.03e-03, 4.15e-03, 3.48e-03, 2.95e-03, 2.53e-03,
             2.20e-03, 1.92e-03, 1.69e-03, 1.50e-03, 1.34e-03, 1.21e-03, 1.09e-03, 9.9e-04,  9.0e-04,  8.3e-04,  7.6e-04,
             7.0e-04,  6.5e-04,  6.0e-04,  5.6e-04,  5.2e-04,  4.9e-04,  4.6e-04,  4.3e-04,  4.1e-04,  3.8e-04,  3.6e-04}, // Conc = 0.3
            {0.0e+00,  0.0e+00,  0.0e+00,  1.575e-02, 1.174e-02, 9.06e-03, 7.18e-03, 5.83e-03, 4.82e-03, 4.04e-03, 3.44e-03,
             2.96e-03, 2.57e-03, 2.26e-03, 1.99e-03,  1.77e-03,  1.59e-03, 1.43e-03, 1.29e-03, 1.17e-03, 1.06e-03, 9.7e-04,
             8.9e-04,  8.2e-04,  7.6e-04,  7.0e-04,   6.5e-04,   6.0e-04,  5.6e-04,  5.3e-04,  4.9e-04,  4.6e-04,  4.3e-04}, // Conc = 0.4
            {6.693e-02, 4.398e-02, 3.050e-02, 2.207e-02, 1.653e-02, 1.274e-02, 1.005e-02, 8.09e-03, 6.63e-03, 5.50e-03, 4.63e-03,
             3.94e-03,  3.39e-03,  2.94e-03,  2.56e-03,  2.26e-03,  2.00e-03,  1.78e-03,  1.59e-03, 1.43e-03, 1.29e-03, 1.17e-03,
             1.07e-03,  9.8e-04,   8.9e-04,   8.2e-04,   7.6e-04,   7.0e-04,   6.5e-04,   6.0e-04,  5.6e-04,  5.3e-04,  4.9e-04}, // Conc = 0.5
            {9.344e-02, 6.525e-02, 4.675e-02, 3.428e-02, 2.569e-02, 1.962e-02, 1.525e-02, 1.205e-02, 9.66e-03, 7.85e-03, 6.46e-03,
             5.38e-03,  4.52e-03,  3.84e-03,  3.29e-03,  2.84e-03,  2.47e-03,  2.16e-03,  1.91e-03,  1.69e-03, 1.51e-03, 1.35e-03,
             1.22e-03,  1.10e-03,  1.00e-03,  9.2e-04,   8.4e-04,   7.7e-04,   7.1e-04,   6.6e-04,   6.1e-04,  5.7e-04,  5.3e-04}, // Conc = 0.6
            {1.3353e-01, 9.657e-02, 7.038e-02, 5.194e-02, 3.888e-02, 2.953e-02, 2.276e-02, 1.779e-02, 1.409e-02, 1.131e-02, 9.18e-03,
             7.53e-03,   6.24e-03,  5.23e-03,  4.42e-03,  3.76e-03,  3.23e-03,  2.80e-03,  2.43e-03,  2.13e-03,  1.88e-03,  1.67e-03,
             1.49e-03,   1.33e-03,  1.20e-03,  1.09e-03,  9.9e-04,   9.0e-04,   8.2e-04,   7.6e-04,   7.0e-04,   6.4e-04,   6.0e-04}, // Conc = 0.7
            {1.9109e-01, 1.4102e-01, 1.0221e-01, 7.453e-02, 5.509e-02, 4.136e-02, 3.156e-02, 2.444e-02, 1.920e-02, 1.529e-02, 1.233e-02,
             1.005e-02,  8.29e-03,   6.90e-03,   5.79e-03,  4.91e-03,  4.19e-03,  3.61e-03,  3.12e-03,  2.72e-03,  2.39e-03,  2.11e-03,
             1.87e-03,   1.66e-03,   1.49e-03,   1.34e-03,  1.21e-03,  1.10e-03,  1.00e-03,  9.1e-04,   8.3e-04,   7.7e-04,   7.1e-04}, // Conc = 0.8
            {0.0e+00,   0.0e+00,   1.9687e-01, 1.2843e-01, 8.752e-02, 6.185e-02, 4.508e-02, 3.374e-02, 2.584e-02, 2.018e-02, 1.604e-02,
             1.295e-02, 1.059e-02, 8.77e-03,   7.34e-03,   6.21e-03,  5.30e-03,  4.56e-03,  3.95e-03,  3.45e-03,  3.03e-03,  2.67e-03,
             2.37e-03,  2.12e-03,  1.90e-03,   1.71e-03,   1.54e-03,  1.40e-03,  1.27e-03,  1.16e-03,  1.07e-03,  9.8e-04,   9.0e-04} // Conc = 0.9
        }};

        // Ethylene Glycol Data: Density in kg/m3
        constexpr std::array<std::array<Real64, DefaultNumGlyTemps>, DefaultNumGlyConcs> DefaultEthGlyRhoData = {{
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   999.8, 999.9, 999.7, 999.1, 998.2, 997.0, 995.6, 994.0, 992.2, 990.2,
             988.0, 985.7, 983.2, 980.5, 977.7, 974.8, 971.8, 968.6, 965.3, 961.9, 958.3, 0.0,   0.0,   0.0,   0.0,   0.0}, // DefaultWaterRhoData
            {0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    1018.73, 1017.57, 1016.28, 1014.87,
             1013.34, 1011.69, 1009.92, 1008.02, 1006.01, 1003.87, 1001.61, 999.23,  996.72,  994.10,  991.35,
             988.49,  985.50,  982.39,  979.15,  975.80,  972.32,  968.73,  965.01,  961.17,  957.21,  953.12}, // Conc=0.1
            {0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    1036.85, 1035.67, 1034.36, 1032.94, 1031.39,
             1029.72, 1027.93, 1026.02, 1023.99, 1021.83, 1019.55, 1017.16, 1014.64, 1011.99, 1009.23, 1006.35,
             1003.34, 1000.21, 996.96,  993.59,  990.10,  986.48,  982.75,  978.89,  974.91,  970.81,  966.59}, // Conc=0.2
            {0.00,    0.00,    0.00,    0.00,    0.00,    1054.31, 1053.11, 1051.78, 1050.33, 1048.76, 1047.07,
             1045.25, 1043.32, 1041.26, 1039.08, 1036.78, 1034.36, 1031.36, 1029.15, 1026.36, 1023.45, 1020.42,
             1017.27, 1014.00, 1010.60, 1007.09, 1003.45, 999.69,  995.81,  991.81,  987.68,  983.43,  979.07}, // Conc=0.3
            {0.00,    0.00,    0.00,    1071.98, 1070.87, 1069.63, 1068.28, 1066.80, 1065.21, 1063.49, 1061.65,
             1059.68, 1057.60, 1055.39, 1053.07, 1050.62, 1048.05, 1045.35, 1042.54, 1039.61, 1036.55, 1033.37,
             1030.07, 1026.65, 1023.10, 1019.44, 1015.65, 1011.74, 1007.70, 1003.56, 999.29,  994.90,  990.38}, // Conc=0.4
            {1089.94, 1089.04, 1088.01, 1086.87, 1085.61, 1084.22, 1082.71, 1081.08, 1079.33, 1077.46, 1075.46,
             1073.35, 1071.11, 1068.75, 1066.27, 1063.66, 1060.94, 1058.09, 1055.13, 1052.04, 1048.83, 1045.49,
             1042.04, 1038.46, 1034.77, 1030.95, 1027.01, 1022.95, 1018.76, 1014.46, 1010.03, 1005.48, 1000.81}, // Conc=0.5
            {1104.60, 1103.54, 1102.36, 1101.06, 1099.64, 1098.09, 1096.43, 1094.64, 1092.73, 1090.70, 1088.54,
             1086.27, 1083.87, 1081.35, 1078.71, 1075.95, 1073.07, 1070.06, 1066.94, 1063.69, 1060.32, 1056.83,
             1053.22, 1049.48, 1045.63, 1041.65, 1037.55, 1033.33, 1028.99, 1024.52, 1019.94, 1015.23, 1010.40}, // Conc=0.6
            {1118.61, 1117.38, 1116.04, 1114.58, 1112.99, 1111.28, 1109.45, 1107.50, 1105.43, 1103.23, 1100.92,
             1098.48, 1095.92, 1093.24, 1090.43, 1087.51, 1084.46, 1081.30, 1078.01, 1074.60, 1071.06, 1067.41,
             1063.64, 1059.74, 1055.72, 1051.58, 1047.32, 1042.93, 1038.43, 1033.80, 1029.05, 1024.18, 1019.19}, // Conc=0.7
            {1132.11, 1130.72, 1129.21, 1127.57, 1125.82, 1123.94, 1121.94, 1119.82, 1117.58, 1115.22, 1112.73,
             1110.13, 1107.40, 1104.55, 1101.58, 1098.48, 1095.27, 1091.93, 1088.48, 1084.90, 1081.20, 1077.37,
             1073.43, 1069.36, 1065.18, 1060.87, 1056.44, 1051.88, 1047.21, 1042.41, 1037.50, 1032.46, 1027.30}, // Conc=0.8
            {0.00,    0.00,    1141.87, 1140.07, 1138.14, 1136.09, 1133.91, 1131.62, 1129.20, 1126.67, 1124.01,
             1121.23, 1118.32, 1115.30, 1112.15, 1108.89, 1105.50, 1101.99, 1098.36, 1094.60, 1090.73, 1086.73,
             1082.61, 1078.37, 1074.01, 1069.53, 1064.92, 1060.20, 1055.35, 1050.38, 1045.29, 1040.08, 1034.74}, // Conc=0.9
        }};

        // Ethylene Glycol Data: Conductivity in W/(m-K)
        constexpr std::array<std::array<Real64, DefaultNumGlyTemps>, DefaultNumGlyConcs> DefaultEthGlyCondData =
            {
                {
                    {0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.561,  0.5705, 0.58,  0.5893,
                     0.5984, 0.6072, 0.6155, 0.6233, 0.6306, 0.6373, 0.6436, 0.6492, 0.6543, 0.659, 0.6631,
                     0.6668, 0.67,   0.6728, 0.6753, 0.6773, 0.6791, 0.0,    0.0,    0.0,    0.0,   0.0}, // DefaultWaterCondData
                    {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.511, 0.520, 0.528, 0.537, 0.545, 0.552, 0.559, 0.566, 0.572, 0.577,
                     0.583, 0.588, 0.592, 0.596, 0.600, 0.603, 0.606, 0.608, 0.610, 0.612, 0.613, 0.614, 0.614, 0.614, 0.613, 0.612}, // Conc=0.1
                    {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.460, 0.468, 0.476, 0.483, 0.490, 0.497, 0.503, 0.509, 0.515, 0.520, 0.525,
                     0.529, 0.534, 0.538, 0.541, 0.544, 0.547, 0.549, 0.551, 0.553, 0.555, 0.556, 0.556, 0.557, 0.557, 0.556, 0.555}, // Conc=0.2
                    {0.000, 0.000, 0.000, 0.000, 0.000, 0.415, 0.422, 0.429, 0.436, 0.442, 0.448, 0.453, 0.459, 0.464, 0.469, 0.473, 0.477,
                     0.481, 0.485, 0.488, 0.491, 0.494, 0.496, 0.498, 0.500, 0.501, 0.503, 0.504, 0.504, 0.505, 0.505, 0.504, 0.504}, // Conc=0.3
                    {0.000, 0.000, 0.000, 0.371, 0.377, 0.383, 0.389, 0.395, 0.400, 0.405, 0.410, 0.415, 0.419, 0.424, 0.428, 0.431, 0.435,
                     0.438, 0.441, 0.444, 0.446, 0.449, 0.451, 0.452, 0.454, 0.455, 0.456, 0.457, 0.458, 0.458, 0.458, 0.458, 0.458}, // Conc=0.4
                    {0.328, 0.333, 0.339, 0.344, 0.349, 0.354, 0.359, 0.364, 0.368, 0.373, 0.377, 0.380, 0.384, 0.387, 0.391, 0.394, 0.397,
                     0.399, 0.402, 0.404, 0.406, 0.408, 0.410, 0.411, 0.413, 0.414, 0.415, 0.416, 0.416, 0.417, 0.417, 0.417, 0.417}, // Conc=0.5
                    {0.307, 0.312, 0.316, 0.321, 0.325, 0.329, 0.333, 0.336, 0.340, 0.343, 0.346, 0.349, 0.352, 0.355, 0.358, 0.360, 0.363,
                     0.365, 0.367, 0.369, 0.371, 0.372, 0.374, 0.375, 0.376, 0.377, 0.378, 0.379, 0.379, 0.380, 0.380, 0.380, 0.380}, // Conc=0.6
                    {0.289, 0.293, 0.296, 0.300, 0.303, 0.306, 0.309, 0.312, 0.314, 0.317, 0.320, 0.322, 0.324, 0.327, 0.329, 0.331, 0.332,
                     0.334, 0.336, 0.337, 0.339, 0.340, 0.341, 0.342, 0.343, 0.344, 0.345, 0.346, 0.346, 0.347, 0.347, 0.347, 0.347}, // Conc=0.7
                    {0.274, 0.276, 0.279, 0.281, 0.283, 0.286, 0.288, 0.290, 0.292, 0.294, 0.296, 0.298, 0.299, 0.301, 0.303, 0.304, 0.306,
                     0.307, 0.308, 0.310, 0.311, 0.312, 0.313, 0.314, 0.314, 0.315, 0.316, 0.316, 0.317, 0.317, 0.318, 0.318, 0.318}, // Conc=0.8
                    {0.000, 0.000, 0.263, 0.265, 0.266, 0.268, 0.269, 0.271, 0.272, 0.274, 0.275, 0.276, 0.278, 0.279, 0.280, 0.281, 0.282,
                     0.283, 0.284, 0.285, 0.286, 0.287, 0.288, 0.288, 0.289, 0.290, 0.290, 0.291, 0.291, 0.292, 0.292, 0.293, 0.293}, // Conc=0.9
                }};

        // Propylene Glycol Data: Specific Heat in J/(kg-k)
        constexpr std::array<std::array<Real64, DefaultNumGlyTemps>, DefaultNumGlyConcs> DefaultPropGlyCpData = {
            {
                {0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    4217.0, 4198.0, 4191.0, 4185.0,
                 4181.0, 4179.0, 4180.0, 4180.0, 4180.0, 4180.0, 4181.0, 4183.0, 4185.0, 4188.0, 4192.0,
                 4196.0, 4200.0, 4203.0, 4208.0, 4213.0, 4218.0, 4223.0, 4228.0, 4233.0, 4238.0, 4243.0}, // DefaultWaterCpData in J/kg-K
                {0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    4042.0, 4050.0, 4058.0, 4067.0,
                 4075.0, 4083.0, 4091.0, 4099.0, 4107.0, 4115.0, 4123.0, 4131.0, 4139.0, 4147.0, 4155.0,
                 4163.0, 4171.0, 4179.0, 4187.0, 4195.0, 4203.0, 4211.0, 4219.0, 4227.0, 4235.0, 4243.0}, // Conc=0.1
                {0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    3918.0, 3929.0, 3940.0, 3951.0, 3962.0,
                 3973.0, 3983.0, 3994.0, 4005.0, 4016.0, 4027.0, 4038.0, 4049.0, 4060.0, 4071.0, 4082.0,
                 4093.0, 4104.0, 4115.0, 4126.0, 4136.0, 4147.0, 4158.0, 4169.0, 4180.0, 4191.0, 4202.0}, // Conc=0.2
                {0.0,    0.0,    0.0,    0.0,    0.0,    3765.0, 3779.0, 3793.0, 3807.0, 3820.0, 3834.0,
                 3848.0, 3862.0, 3875.0, 3889.0, 3903.0, 3917.0, 3930.0, 3944.0, 3958.0, 3972.0, 3985.0,
                 3999.0, 4013.0, 4027.0, 4040.0, 4054.0, 4068.0, 4082.0, 4095.0, 4109.0, 4123.0, 4137.0}, // Conc=0.3
                {0.0,    0.0,    0.0,    0.0,    3586.0, 3603.0, 3619.0, 3636.0, 3652.0, 3669.0, 3685.0,
                 3702.0, 3718.0, 3735.0, 3751.0, 3768.0, 3784.0, 3801.0, 3817.0, 3834.0, 3850.0, 3867.0,
                 3883.0, 3900.0, 3916.0, 3933.0, 3949.0, 3966.0, 3982.0, 3999.0, 4015.0, 4032.0, 4049.0}, // Conc=0.4
                {0.0,    0.0,    3358.0, 3378.0, 3397.0, 3416.0, 3435.0, 3455.0, 3474.0, 3493.0, 3513.0,
                 3532.0, 3551.0, 3570.0, 3590.0, 3609.0, 3628.0, 3648.0, 3667.0, 3686.0, 3706.0, 3725.0,
                 3744.0, 3763.0, 3783.0, 3802.0, 3821.0, 3841.0, 3860.0, 3879.0, 3898.0, 3918.0, 3937.0}, // Conc=0.5
                {3096.0, 3118.0, 3140.0, 3162.0, 3184.0, 3206.0, 3228.0, 3250.0, 3272.0, 3295.0, 3317.0,
                 3339.0, 3361.0, 3383.0, 3405.0, 3427.0, 3449.0, 3471.0, 3493.0, 3515.0, 3537.0, 3559.0,
                 3581.0, 3603.0, 3625.0, 3647.0, 3670.0, 3692.0, 3714.0, 3736.0, 3758.0, 3780.0, 3802.0}, // Conc=0.6
                {2843.0, 2868.0, 2893.0, 2918.0, 2943.0, 2968.0, 2993.0, 3018.0, 3042.0, 3067.0, 3092.0,
                 3117.0, 3142.0, 3167.0, 3192.0, 3217.0, 3242.0, 3266.0, 3291.0, 3316.0, 3341.0, 3366.0,
                 3391.0, 3416.0, 3441.0, 3465.0, 3490.0, 3515.0, 3540.0, 3565.0, 3590.0, 3615.0, 3640.0}, // Conc=0.7
                {2572.0, 2600.0, 2627.0, 2655.0, 2683.0, 2710.0, 2738.0, 2766.0, 2793.0, 2821.0, 2849.0,
                 2876.0, 2904.0, 2931.0, 2959.0, 2987.0, 3014.0, 3042.0, 3070.0, 3097.0, 3125.0, 3153.0,
                 3180.0, 3208.0, 3236.0, 3263.0, 3291.0, 3319.0, 3346.0, 3374.0, 3402.0, 3429.0, 3457.0}, // Conc=0.8
                {2264.0, 2295.0, 2326.0, 2356.0, 2387.0, 2417.0, 2448.0, 2478.0, 2509.0, 2539.0, 2570.0,
                 2600.0, 2631.0, 2661.0, 2692.0, 2723.0, 2753.0, 2784.0, 2814.0, 2845.0, 2875.0, 2906.0,
                 2936.0, 2967.0, 2997.0, 3028.0, 3058.0, 3089.0, 3119.0, 3150.0, 3181.0, 3211.0, 3242.0}, // Conc=0.9
            }};

        // Propylene Glycol Data: Viscosity in mPa-s
        constexpr std::array<std::array<Real64, DefaultNumGlyTemps>, DefaultNumGlyConcs> DefaultPropGlyViscData = {{
            {0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,    0.0e-3,   1.7912e-3, 1.5183e-3, 1.306e-3,  1.1376e-3, 1.0016e-3,
             0.8901e-3, 0.7974e-3, 0.7193e-3, 0.653e-3,  0.5961e-3, 0.5468e-3, 0.504e-3, 0.4664e-3, 0.4332e-3, 0.4039e-3, 0.3777e-3, 0.3543e-3,
             0.3333e-3, 0.3144e-3, 0.2973e-3, 0.2817e-3, 0.0e-3,    0.0e-3,    0.0e-3,   0.0e-3,    0.0e-3}, // DefaultWaterViscData in Pa-s
            {0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00, 0.0e+00, 0.0e+00, 0.0e+00, 2.68e-03, 2.23e-03, 1.89e-03, 1.63e-03,
             1.42e-03, 1.25e-03, 1.11e-03, 9.9e-04, 8.9e-04, 8.1e-04, 7.3e-04, 6.7e-04,  6.2e-04,  5.7e-04,  5.3e-04,
             4.9e-04,  4.6e-04,  4.3e-04,  4.0e-04, 3.8e-04, 3.5e-04, 3.3e-04, 3.2e-04,  3.0e-04,  2.8e-04,  2.7e-04}, // Conc=0.1
            {0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  4.98e-03, 4.05e-03, 3.34e-03, 2.79e-03, 2.36e-03,
             2.02e-03, 1.74e-03, 1.52e-03, 1.34e-03, 1.18e-03, 1.06e-03, 9.5e-04,  8.6e-04,  7.8e-04,  7.1e-04,  6.6e-04,
             6.0e-04,  5.6e-04,  5.2e-04,  4.9e-04,  4.5e-04,  4.3e-04,  4.0e-04,  3.8e-04,  3.6e-04,  3.4e-04,  3.2e-04}, // Conc=0.2
            {0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  1.187e-02, 9.08e-03, 7.08e-03, 5.61e-03, 4.52e-03, 3.69e-03,
             3.06e-03, 2.57e-03, 2.18e-03, 1.88e-03, 1.63e-03, 1.43e-03,  1.26e-03, 1.13e-03, 1.01e-03, 9.1e-04,  8.3e-04,
             7.6e-04,  7.0e-04,  6.5e-04,  6.1e-04,  5.7e-04,  5.3e-04,   5.0e-04,  4.7e-04,  4.5e-04,  4.3e-04,  4.1e-04}, // Conc=0.3
            {0.0e+00,  0.0e+00,  0.0e+00,  0.0e+00,  3.322e-02, 2.327e-02, 1.675e-02, 1.237e-02, 9.35e-03, 7.22e-03, 5.69e-03,
             4.57e-03, 3.73e-03, 3.09e-03, 2.60e-03, 2.21e-03,  1.91e-03,  1.66e-03,  1.47e-03,  1.30e-03, 1.17e-03, 1.06e-03,
             9.6e-04,  8.8e-04,  8.1e-04,  7.5e-04,  7.0e-04,   6.6e-04,   6.2e-04,   5.9e-04,   5.6e-04,  5.3e-04,  5.1e-04}, // Conc=0.4
            {0.0e+00,  0.0e+00,  1.1059e-01, 7.303e-02, 4.970e-02, 3.478e-02, 2.499e-02, 1.840e-02, 1.385e-02, 1.065e-02, 8.34e-03,
             6.65e-03, 5.39e-03, 4.43e-03,   3.69e-03,  3.11e-03,  2.65e-03,  2.29e-03,  1.99e-03,  1.75e-03,  1.55e-03,  1.38e-03,
             1.24e-03, 1.12e-03, 1.02e-03,   9.3e-04,   8.6e-04,   7.9e-04,   7.4e-04,   6.9e-04,   6.4e-04,   6.0e-04,   5.7e-04}, // Conc=0.5
            {5.2401e-01, 3.3039e-01, 2.1143e-01, 1.3796e-01, 9.200e-02, 6.278e-02, 4.384e-02, 3.132e-02, 2.287e-02, 1.705e-02, 1.296e-02,
             1.004e-02,  7.91e-03,   6.34e-03,   5.15e-03,   4.25e-03,  3.55e-03,  3.00e-03,  2.57e-03,  2.22e-03,  1.93e-03,  1.70e-03,
             1.51e-03,   1.35e-03,   1.22e-03,   1.10e-03,   1.01e-03,  9.2e-04,   8.5e-04,   7.9e-04,   7.4e-04,   6.9e-04,   6.5e-04}, // Conc=0.6
            {9.1618e-01, 5.5112e-01, 3.4009e-01, 2.1567e-01, 1.4062e-01, 9.423e-02, 6.483e-02, 4.574e-02, 3.304e-02, 2.441e-02, 1.841e-02,
             1.415e-02,  1.108e-02,  8.81e-03,   7.12e-03,   5.84e-03,   4.85e-03,  4.08e-03,  3.46e-03,  2.98e-03,  2.58e-03,  2.26e-03,
             1.99e-03,   1.77e-03,   1.59e-03,   1.43e-03,   1.30e-03,   1.18e-03,  1.08e-03,  1.00e-03,  9.3e-04,   8.6e-04,   8.0e-04}, // Conc=0.7
            {1.43422e+00, 9.0847e-01, 5.7592e-01, 3.6877e-01, 2.3986e-01, 1.5902e-01, 1.0764e-01, 7.445e-02, 5.263e-02,
             3.799e-02,   2.800e-02,  2.104e-02,  1.610e-02,  1.255e-02,  9.94e-03,   7.99e-03,   6.52e-03,  5.39e-03,
             4.51e-03,    3.82e-03,   3.28e-03,   2.83e-03,   2.47e-03,   2.18e-03,   1.94e-03,   1.73e-03,  1.56e-03,
             1.42e-03,    1.29e-03,   1.19e-03,   1.09e-03,   1.02e-03,   9.5e-04}, // Conc=0.8
            {3.81329e+00, 2.07134e+00, 1.17609e+00, 6.9609e-01, 4.2819e-01, 2.7294e-01, 1.7978e-01, 1.2203e-01, 8.515e-02,
             6.093e-02,   4.462e-02,   3.338e-02,   2.545e-02,  1.976e-02,  1.560e-02,  1.249e-02,  1.015e-02,  8.35e-03,
             6.95e-03,    5.85e-03,    4.97e-03,    4.26e-03,   3.69e-03,   3.22e-03,   2.83e-03,   2.50e-03,   2.23e-03,
             2.00e-03,    1.80e-03,    1.63e-03,    1.48e-03,   1.35e-03,   1.24e-03}, // Conc=0.9
        }};

        // Propylene Glycol Data: Density in kg/m3
        constexpr std::array<std::array<Real64, DefaultNumGlyTemps>, DefaultNumGlyConcs> DefaultPropGlyRhoData = {{
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   999.8, 999.9, 999.7, 999.1, 998.2, 997.0, 995.6, 994.0, 992.2, 990.2,
             988.0, 985.7, 983.2, 980.5, 977.7, 974.8, 971.8, 968.6, 965.3, 961.9, 958.3, 0.0,   0.0,   0.0,   0.0,   0.0}, // DefaultWaterRhoData in
                                                                                                                            // kg/m3
            {0.00,    0.00,    0.00,    0.00,    0.00,    0.00,   0.00,   1013.85, 1012.61, 1011.24, 1009.75,
             1008.13, 1006.40, 1004.54, 1002.56, 1000.46, 998.23, 995.88, 993.41,  990.82,  988.11,  985.27,
             982.31,  979.23,  976.03,  972.70,  969.25,  965.68, 961.99, 958.17,  954.24,  950.18,  945.99}, // Conc=0.1
            {0.00,    0.00,    0.00,    0.00,    0.00,    0.00,    1027.24, 1025.84, 1024.32, 1022.68, 1020.91,
             1019.01, 1016.99, 1014.84, 1012.56, 1010.16, 1007.64, 1004.99, 1002.21, 999.31,  996.28,  993.12,
             989.85,  986.44,  982.91,  979.25,  975.47,  971.56,  967.53,  963.37,  959.09,  954.67,  950.14}, // Conc=0.2
            {0.00,    0.00,    0.00,    0.00,    0.00,    1039.42, 1037.89, 1036.24, 1034.46, 1032.55, 1030.51,
             1028.35, 1026.06, 1023.64, 1021.09, 1018.42, 1015.62, 1012.69, 1009.63, 1006.44, 1003.13, 999.69,
             996.12,  992.42,  988.60,  984.65,  980.57,  976.36,  972.03,  967.56,  962.97,  958.26,  953.41}, // Conc=0.3
            {0.00,    0.00,    0.00,    0.00,    1050.43, 1048.79, 1047.02, 1045.12, 1043.09, 1040.94, 1038.65,
             1036.24, 1033.70, 1031.03, 1028.23, 1025.30, 1022.24, 1019.06, 1015.75, 1012.30, 1008.73, 1005.03,
             1001.21, 997.25,  993.17,  988.95,  984.61,  980.14,  975.54,  970.81,  965.95,  960.97,  955.86}, // Conc=0.4
            {0.00,    0.00,    1062.11, 1060.49, 1058.73, 1056.85, 1054.84, 1052.71, 1050.44, 1048.04, 1045.52,
             1042.87, 1040.09, 1037.18, 1034.15, 1030.98, 1027.69, 1024.27, 1020.72, 1017.04, 1013.23, 1009.30,
             1005.24, 1001.05, 996.73,  992.28,  987.70,  983.00,  978.16,  973.20,  968.11,  962.89,  957.55}, // Conc=0.5
            {1072.92, 1071.31, 1069.58, 1067.72, 1065.73, 1063.61, 1061.37, 1059.00, 1056.50, 1053.88, 1051.13,
             1048.25, 1045.24, 1042.11, 1038.85, 1035.47, 1031.95, 1028.32, 1024.55, 1020.66, 1016.63, 1012.49,
             1008.21, 1003.81, 999.28,  994.63,  989.85,  984.94,  979.90,  974.74,  969.45,  964.03,  958.49}, // Conc=0.6
            {1079.67, 1077.82, 1075.84, 1073.74, 1071.51, 1069.16, 1066.69, 1064.09, 1061.36, 1058.51, 1055.54,
             1052.44, 1049.22, 1045.87, 1042.40, 1038.81, 1035.09, 1031.25, 1027.28, 1023.19, 1018.97, 1014.63,
             1010.16, 1005.57, 1000.86, 996.02,  991.06,  985.97,  980.76,  975.42,  969.96,  964.38,  958.67}, // Conc=0.7
            {1094.50, 1090.85, 1087.18, 1083.49, 1079.77, 1076.04, 1072.27, 1068.49, 1064.68, 1060.85, 1057.00,
             1053.12, 1049.22, 1045.30, 1041.35, 1037.38, 1033.39, 1029.37, 1025.33, 1021.27, 1017.19, 1013.08,
             1008.95, 1004.79, 1000.62, 996.41,  992.19,  987.94,  983.68,  979.38,  975.07,  970.73,  966.37}, // Conc=0.8
            {1092.46, 1088.82, 1085.15, 1081.46, 1077.74, 1074.00, 1070.24, 1066.46, 1062.65, 1058.82, 1054.96,
             1051.09, 1047.19, 1043.26, 1039.32, 1035.35, 1031.35, 1027.34, 1023.30, 1019.24, 1015.15, 1011.04,
             1006.91, 1002.76, 998.58,  994.38,  990.16,  985.91,  981.64,  977.35,  973.03,  968.69,  964.33}, // Conc=0.9
        }};

        // Propylene Glycol Data: Conductivity in W/(m-K)
        constexpr std::array<std::array<Real64, DefaultNumGlyTemps>, DefaultNumGlyConcs> DefaultPropGlyCondData =
            {
                {
                    {0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.561,  0.5705, 0.58,  0.5893,
                     0.5984, 0.6072, 0.6155, 0.6233, 0.6306, 0.6373, 0.6436, 0.6492, 0.6543, 0.659, 0.6631,
                     0.6668, 0.67,   0.6728, 0.6753, 0.6773, 0.6791, 0.0,    0.0,    0.0,    0.0,   0.0}, // DefaultWaterCondData
                                                                                                          // in W/mK
                    {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.510, 0.518, 0.527, 0.535, 0.543, 0.550, 0.557, 0.563, 0.569, 0.575,
                     0.580, 0.585, 0.589, 0.593, 0.596, 0.599, 0.602, 0.604, 0.606, 0.607, 0.608, 0.609, 0.609, 0.608, 0.608, 0.606}, // Conc=0.1
                    {0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.456, 0.464, 0.472, 0.479, 0.485, 0.492, 0.498, 0.503, 0.508, 0.513, 0.518,
                     0.522, 0.526, 0.529, 0.532, 0.535, 0.538, 0.540, 0.541, 0.543, 0.544, 0.544, 0.544, 0.544, 0.544, 0.543, 0.542}, // Conc=0.2
                    {0.000, 0.000, 0.000, 0.000, 0.000, 0.410, 0.416, 0.423, 0.429, 0.434, 0.440, 0.445, 0.449, 0.454, 0.458, 0.462, 0.466,
                     0.469, 0.472, 0.475, 0.477, 0.479, 0.481, 0.482, 0.484, 0.484, 0.485, 0.485, 0.485, 0.485, 0.485, 0.484, 0.482}, // Conc=0.3
                    {0.000, 0.000, 0.000, 0.000, 0.369, 0.375, 0.380, 0.385, 0.389, 0.394, 0.398, 0.402, 0.406, 0.409, 0.412, 0.415, 0.418,
                     0.420, 0.423, 0.425, 0.426, 0.428, 0.429, 0.430, 0.431, 0.431, 0.432, 0.432, 0.432, 0.431, 0.430, 0.429, 0.428}, // Conc=0.4
                    {0.000, 0.000, 0.329, 0.334, 0.338, 0.342, 0.346, 0.349, 0.353, 0.356, 0.359, 0.362, 0.365, 0.367, 0.370, 0.372, 0.374,
                     0.375, 0.377, 0.378, 0.379, 0.380, 0.381, 0.382, 0.382, 0.382, 0.382, 0.382, 0.382, 0.381, 0.380, 0.379, 0.378}, // Conc=0.5
                    {0.296, 0.300, 0.303, 0.306, 0.309, 0.312, 0.314, 0.317, 0.319, 0.321, 0.323, 0.325, 0.327, 0.329, 0.330, 0.331, 0.333,
                     0.334, 0.335, 0.335, 0.336, 0.336, 0.337, 0.337, 0.337, 0.337, 0.336, 0.336, 0.335, 0.335, 0.334, 0.333, 0.332}, // Conc=0.6
                    {0.275, 0.277, 0.278, 0.280, 0.282, 0.284, 0.285, 0.286, 0.289, 0.290, 0.291, 0.292, 0.293, 0.293, 0.294, 0.294, 0.295,
                     0.295, 0.295, 0.295, 0.295, 0.295, 0.295, 0.295, 0.295, 0.294, 0.294, 0.293, 0.292, 0.292, 0.291, 0.290, 0.288}, // Conc=0.7
                    {0.255, 0.256, 0.257, 0.257, 0.258, 0.259, 0.259, 0.259, 0.260, 0.260, 0.260, 0.261, 0.261, 0.261, 0.261, 0.261, 0.260,
                     0.260, 0.260, 0.260, 0.259, 0.259, 0.258, 0.258, 0.257, 0.256, 0.256, 0.255, 0.254, 0.253, 0.252, 0.251, 0.250}, // Conc=0.8
                    {0.237, 0.237, 0.236, 0.236, 0.236, 0.235, 0.235, 0.234, 0.234, 0.233, 0.233, 0.232, 0.233, 0.231, 0.230, 0.229, 0.229,
                     0.228, 0.227, 0.227, 0.226, 0.225, 0.224, 0.223, 0.222, 0.221, 0.220, 0.219, 0.218, 0.217, 0.216, 0.215, 0.214}, // Conc=0.9
                }};

        // Steam Refrigerant Data
        constexpr std::array<Real64, DefaultNumSteamTemps> DefaultSteamTemps{
            1.00e-002, 1.0,   5.0,   10.0,  15.0,  20.0,  25.0,  30.0,  35.0,  40.0,  45.0,  50.0,  55.0,  60.0,  65.0,  70.0,  72.0,  74.0,  76.0,
            78.0,      80.0,  82.0,  84.0,  86.0,  88.0,  90.0,  92.0,  94.0,  96.0,  98.0,  99.0,  100.0, 101.0, 102.0, 103.0, 104.0, 105.0, 106.0,
            107.0,     108.0, 109.0, 110.0, 111.0, 112.0, 113.0, 114.0, 115.0, 116.0, 117.0, 118.0, 119.0, 120.0, 121.0, 122.0, 123.0, 124.0, 125.0,
            126.0,     127.0, 128.0, 129.0, 130.0, 132.0, 134.0, 136.0, 138.0, 140.0, 142.0, 144.0, 146.0, 148.0, 150.0, 152.0, 154.0, 156.0, 158.0,
            160.0,     162.0, 164.0, 166.0, 168.0, 170.0, 172.0, 174.0, 176.0, 178.0, 180.0, 185.0, 190.0, 195.0, 200.0, 205.0, 210.0, 215.0, 220.0,
            225.0,     230.0, 240.0, 250.0, 260.0, 270.0, 280.0, 290.0, 300.0, 310.0, 320.0, 330.0, 340.0, 350.0, 360.0, 370.0};

        constexpr std::array<Real64, DefaultNumSteamTemps> DefaultSteamPressData{
            611.7,      657.1,      872.6,     1228.0,    1706.0,    2339.0,    3170.0,    4247.0,    5629.0,    7385.0,     9595.0,     12350.0,
            15760.0,    19950.0,    25040.0,   31200.0,   34000.0,   37010.0,   40240.0,   43700.0,   47410.0,   51390.0,    55640.0,    60170.0,
            65020.0,    70180.0,    75680.0,   81540.0,   87770.0,   94390.0,   97850.0,   101400.0,  105100.0,  108900.0,   112800.0,   116800.0,
            120900.0,   125100.0,   129500.0,  134000.0,  138600.0,  143400.0,  148300.0,  153300.0,  158400.0,  163700.0,   169200.0,   174800.0,
            180500.0,   186400.0,   192500.0,  198700.0,  205000.0,  211600.0,  218300.0,  225200.0,  232200.0,  239500.0,   246900.0,   254500.0,
            262300.0,   270300.0,   286800.0,  304200.0,  322400.0,  341500.0,  361500.0,  382500.0,  404400.0,  427300.0,   451200.0,   476200.0,
            502200.0,   529500.0,   557800.0,  587400.0,  618200.0,  650300.0,  683700.0,  718500.0,  754600.0,  792200.0,   831200.0,   871800.0,
            913800.0,   957500.0,   1003000.0, 1123000.0, 1255000.0, 1399000.0, 1555000.0, 1724000.0, 1908000.0, 2106000.0,  2320000.0,  2550000.0,
            2797000.0,  3347000.0,  3976000.0, 4692000.0, 5503000.0, 6417000.0, 7442000.0, 8588000.0, 9865000.0, 11280000.0, 12860000.0, 14600000.0,
            16530000.0, 18670000.0, 21040000.0};

        constexpr std::array<Real64, DefaultNumSteamTemps> DefaultSteamEnthalpyFluidData{
            0.59,      4177.0,    21020.0,   42020.0,   62980.0,   83910.0,   104800.0,  125700.0,  146600.0,  167500.0,  188400.0,  209300.0,
            230300.0,  251200.0,  272100.0,  293100.0,  301400.0,  309800.0,  318200.0,  326600.0,  335000.0,  343400.0,  351800.0,  360200.0,
            368600.0,  377000.0,  385500.0,  393900.0,  402300.0,  410700.0,  414900.0,  419200.0,  423400.0,  427600.0,  431800.0,  436000.0,
            440300.0,  444500.0,  448700.0,  453000.0,  457200.0,  461400.0,  465600.0,  469900.0,  474100.0,  478400.0,  482600.0,  486800.0,
            491100.0,  495300.0,  499600.0,  503800.0,  508100.0,  512300.0,  516600.0,  520800.0,  525100.0,  529300.0,  533600.0,  537900.0,
            542100.0,  546400.0,  554900.0,  563500.0,  572000.0,  580600.0,  589200.0,  597700.0,  606300.0,  614900.0,  623600.0,  632200.0,
            640800.0,  649500.0,  658100.0,  666800.0,  675500.0,  684200.0,  692900.0,  701600.0,  710300.0,  719100.0,  727800.0,  736600.0,
            745400.0,  754200.0,  763100.0,  785200.0,  807400.0,  829800.0,  852300.0,  874900.0,  897600.0,  920500.0,  943600.0,  966800.0,
            990200.0,  1038000.0, 1086000.0, 1135000.0, 1185000.0, 1237000.0, 1290000.0, 1345000.0, 1402000.0, 1462000.0, 1526000.0, 1595000.0,
            1671000.0, 1762000.0, 1891000.0};

        constexpr std::array<Real64, DefaultNumSteamTemps> DefaultSteamEnthalpyGasFluidData{
            2501000.0, 2503000.0, 2510000.0, 2519000.0, 2528000.0, 2537000.0, 2547000.0, 2556000.0, 2565000.0, 2574000.0, 2582000.0, 2591000.0,
            2600000.0, 2609000.0, 2618000.0, 2626000.0, 2630000.0, 2633000.0, 2636000.0, 2640000.0, 2643000.0, 2646000.0, 2650000.0, 2653000.0,
            2656000.0, 2660000.0, 2663000.0, 2666000.0, 2669000.0, 2672000.0, 2674000.0, 2676000.0, 2677000.0, 2679000.0, 2680000.0, 2682000.0,
            2683000.0, 2685000.0, 2686000.0, 2688000.0, 2690000.0, 2691000.0, 2693000.0, 2694000.0, 2696000.0, 2697000.0, 2699000.0, 2700000.0,
            2702000.0, 2703000.0, 2704000.0, 2706000.0, 2707000.0, 2709000.0, 2710000.0, 2712000.0, 2713000.0, 2715000.0, 2716000.0, 2717000.0,
            2719000.0, 2720000.0, 2723000.0, 2726000.0, 2728000.0, 2731000.0, 2733000.0, 2736000.0, 2739000.0, 2741000.0, 2744000.0, 2746000.0,
            2748000.0, 2751000.0, 2753000.0, 2755000.0, 2757000.0, 2760000.0, 2762000.0, 2764000.0, 2766000.0, 2768000.0, 2770000.0, 2772000.0,
            2774000.0, 2775000.0, 2777000.0, 2781000.0, 2785000.0, 2789000.0, 2792000.0, 2795000.0, 2797000.0, 2799000.0, 2801000.0, 2802000.0,
            2803000.0, 2803000.0, 2801000.0, 2797000.0, 2790000.0, 2780000.0, 2767000.0, 2750000.0, 2728000.0, 2701000.0, 2666000.0, 2622000.0,
            2564000.0, 2481000.0, 2335000.0};

        constexpr std::array<Real64, DefaultNumSteamTemps> DefaultSteamCpFluidData{
            4220.0, 4217.0, 4205.0, 4196.0, 4189.0, 4184.0, 4182.0, 4180.0, 4180.0, 4180.0, 4180.0, 4182.0, 4183.0,  4185.0,  4187.0, 4190.0,
            4191.0, 4193.0, 4194.0, 4195.0, 4197.0, 4198.0, 4200.0, 4202.0, 4203.0, 4205.0, 4207.0, 4209.0, 4211.0,  4213.0,  4215.0, 4216.0,
            4217.0, 4218.0, 4219.0, 4220.0, 4222.0, 4223.0, 4224.0, 4226.0, 4227.0, 4228.0, 4230.0, 4231.0, 4233.0,  4234.0,  4236.0, 4237.0,
            4239.0, 4240.0, 4242.0, 4244.0, 4245.0, 4247.0, 4249.0, 4250.0, 4252.0, 4254.0, 4256.0, 4258.0, 4260.0,  4261.0,  4265.0, 4270.0,
            4274.0, 4278.0, 4283.0, 4287.0, 4292.0, 4297.0, 4302.0, 4307.0, 4312.0, 4318.0, 4324.0, 4329.0, 4335.0,  4341.0,  4348.0, 4354.0,
            4361.0, 4368.0, 4375.0, 4382.0, 4390.0, 4397.0, 4405.0, 4425.0, 4447.0, 4471.0, 4496.0, 4523.0, 4551.0,  4582.0,  4615.0, 4650.0,
            4688.0, 4772.0, 4870.0, 4986.0, 5123.0, 5289.0, 5493.0, 5750.0, 6085.0, 6537.0, 7186.0, 8208.0, 10120.0, 15000.0, 45160.0};

        constexpr std::array<Real64, DefaultNumSteamTemps> DefaultSteamCpGasFluidData{
            1884.0, 1885.0, 1889.0, 1895.0, 1900.0, 1906.0, 1912.0, 1918.0, 1925.0, 1931.0, 1939.0, 1947.0,  1955.0,  1965.0,  1975.0, 1986.0,
            1991.0, 1996.0, 2001.0, 2006.0, 2012.0, 2018.0, 2024.0, 2030.0, 2036.0, 2043.0, 2050.0, 2057.0,  2064.0,  2072.0,  2076.0, 2080.0,
            2084.0, 2088.0, 2093.0, 2097.0, 2101.0, 2106.0, 2110.0, 2115.0, 2120.0, 2124.0, 2129.0, 2134.0,  2139.0,  2144.0,  2150.0, 2155.0,
            2160.0, 2166.0, 2171.0, 2177.0, 2183.0, 2189.0, 2195.0, 2201.0, 2207.0, 2213.0, 2219.0, 2226.0,  2232.0,  2239.0,  2252.0, 2266.0,
            2281.0, 2296.0, 2311.0, 2327.0, 2343.0, 2359.0, 2376.0, 2394.0, 2412.0, 2430.0, 2449.0, 2468.0,  2488.0,  2509.0,  2529.0, 2551.0,
            2572.0, 2594.0, 2617.0, 2640.0, 2664.0, 2688.0, 2713.0, 2777.0, 2844.0, 2915.0, 2990.0, 3068.0,  3150.0,  3237.0,  3329.0, 3426.0,
            3528.0, 3754.0, 4011.0, 4308.0, 4656.0, 5073.0, 5582.0, 6220.0, 7045.0, 8159.0, 9753.0, 12240.0, 16690.0, 27360.0, 96600.0};

        constexpr std::array<Real64, DefaultNumSteamTemps> DefaultSteamDensityFluidData{
            999.8, 999.9, 999.9, 999.7, 999.1, 998.2, 997.0, 995.6, 994.0, 992.2, 990.2, 988.0, 985.7, 983.2, 980.5, 977.7, 976.6, 975.4, 974.2,
            973.0, 971.8, 970.5, 969.2, 967.9, 966.6, 965.3, 963.9, 962.6, 961.2, 959.8, 959.1, 958.3, 957.6, 956.9, 956.2, 955.4, 954.7, 954.0,
            953.2, 952.5, 951.7, 950.9, 950.2, 949.4, 948.6, 947.9, 947.1, 946.3, 945.5, 944.7, 943.9, 943.1, 942.3, 941.5, 940.7, 939.8, 939.0,
            938.2, 937.4, 936.5, 935.7, 934.8, 933.1, 931.4, 929.7, 927.9, 926.1, 924.3, 922.5, 920.7, 918.9, 917.0, 915.1, 913.2, 911.3, 909.4,
            907.4, 905.5, 903.5, 901.5, 899.5, 897.5, 895.4, 893.3, 891.2, 889.1, 887.0, 881.6, 876.1, 870.4, 864.7, 858.8, 852.7, 846.5, 840.2,
            833.7, 827.1, 813.4, 798.9, 783.6, 767.5, 750.3, 731.9, 712.1, 690.7, 667.1, 640.8, 610.7, 574.7, 527.6, 451.4};

        constexpr std::array<Real64, DefaultNumSteamTemps> DefaultSteamDensityGasFluidData{
            4.86e-003, 5.20e-003, 6.80e-003, 9.41e-003, 1.28e-002, 1.73e-002, 2.31e-002, 3.04e-002, 3.97e-002, 5.12e-002, 6.56e-002, 8.32e-002, 0.10,
            0.13,      0.16,      0.20,      0.22,      0.23,      0.25,      0.27,      0.29,      0.32,      0.34,      0.37,      0.39,      0.42,
            0.45,      0.49,      0.52,      0.56,      0.58,      0.60,      0.62,      0.64,      0.66,      0.68,      0.71,      0.73,      0.75,
            0.78,      0.80,      0.83,      0.85,      0.88,      0.91,      0.94,      0.97,      1.00,      1.03,      1.06,      1.09,      1.12,
            1.16,      1.19,      1.23,      1.26,      1.30,      1.34,      1.38,      1.42,      1.46,      1.50,      1.58,      1.67,      1.77,
            1.86,      1.97,      2.07,      2.19,      2.30,      2.42,      2.55,      2.68,      2.82,      2.96,      3.11,      3.26,      3.42,
            3.59,      3.76,      3.94,      4.12,      4.32,      4.52,      4.72,      4.94,      5.16,      5.75,      6.40,      7.10,      7.86,
            8.69,      9.59,      10.56,     11.62,     12.75,     13.99,     16.75,     19.97,     23.71,     28.07,     33.16,     39.13,     46.17,
            54.54,     64.64,     77.05,     92.76,     113.60,    143.90,    201.80};

        constexpr std::array<Real64, DefaultNumSteamSuperheatedTemps> DefaultSteamSuperheatedTemps{
            1.00e-002, 1.0,   5.0,   10.0,  15.0,  20.0,  25.0,  30.0,  35.0,  40.0,  45.0,  50.0,  55.0,  60.0,  65.0,  70.0,  72.0,  74.0,  76.0,
            78.0,      80.0,  82.0,  84.0,  86.0,  88.0,  90.0,  92.0,  94.0,  96.0,  98.0,  99.0,  100.0, 101.0, 102.0, 103.0, 104.0, 105.0, 106.0,
            107.0,     108.0, 109.0, 110.0, 111.0, 112.0, 113.0, 114.0, 115.0, 116.0, 117.0, 118.0, 119.0, 120.0, 121.0, 122.0, 123.0, 124.0, 125.0,
            126.0,     127.0, 128.0, 129.0, 130.0, 132.0, 134.0, 136.0, 138.0, 140.0, 142.0, 144.0, 146.0, 148.0, 150.0, 152.0, 154.0, 156.0, 158.0,
            160.0,     162.0, 164.0, 166.0, 168.0, 170.0, 172.0, 174.0, 176.0, 178.0, 180.0, 185.0, 190.0, 195.0, 200.0, 205.0, 210.0, 215.0, 220.0,
            225.0,     230.0, 240.0, 250.0, 260.0, 270.0, 280.0, 290.0, 300.0, 310.0, 320.0, 330.0, 340.0, 350.0, 360.0, 370.0, 400.0, 450.0, 500.0};

        constexpr std::array<Real64, DefaultNumSteamSuperheatedTemps> DefaultSteamSuperheatedPressData{
            611.70,     657.10,     872.60,     1228.0,    1706.0,    2339.0,    3170.0,     4247.0,     5629.0,     7385.0,     9595.0,
            12350.0,    15760.0,    19950.0,    25040.0,   31200.0,   34000.0,   37010.0,    40240.0,    43700.0,    47410.0,    51390.0,
            55640.0,    60170.0,    65020.0,    70180.0,   75680.0,   81540.0,   87770.0,    94390.0,    97850.0,    101400.0,   105100.0,
            108900.0,   112800.0,   116800.0,   120900.0,  125100.0,  129500.0,  134000.0,   138600.0,   143400.0,   148300.0,   153300.0,
            158400.0,   163700.0,   169200.0,   174800.0,  180500.0,  186400.0,  192500.0,   198700.0,   205000.0,   211600.0,   218300.0,
            225200.0,   232200.0,   239500.0,   246900.0,  254500.0,  262300.0,  270300.0,   286800.0,   304200.0,   322400.0,   341500.0,
            361500.0,   382500.0,   404400.0,   427300.0,  451200.0,  476200.0,  502200.0,   529500.0,   557800.0,   587400.0,   618200.0,
            650300.0,   683700.0,   718500.0,   754600.0,  792200.0,  831200.0,  871800.0,   913800.0,   957500.0,   1003000.0,  1123000.0,
            1255000.0,  1399000.0,  1555000.0,  1724000.0, 1908000.0, 2106000.0, 2320000.0,  2550000.0,  2797000.0,  3347000.0,  3976000.0,
            4692000.0,  5503000.0,  6417000.0,  7442000.0, 8588000.0, 9865000.0, 11280000.0, 12860000.0, 14600000.0, 16530000.0, 18670000.0,
            21040000.0, 30000000.0, 35000000.0, 40000000.0};

        Array2D<Real64> DefaultSteamSuperheatedEnthalpyData(DefaultNumSteamSuperheatedPressure, DefaultNumSteamSuperheatedTemps);

        Array2D<Real64> DefaultSteamSuperheatedDensityData(DefaultNumSteamSuperheatedPressure, DefaultNumSteamSuperheatedTemps);

        struct FluidTempData
        {
            // Members
            std::string Name;      // Name of the temperature list
            int NumOfTemps;        // Number of temperatures in a particular arry
            Array1D<Real64> Temps; // Temperature values (degrees C)

            // Default Constructor
            FluidTempData() : NumOfTemps(0)
            {
            }
        };

        struct PressureSequence
        {
            // Members
            Real64 Pressure;
            int InPtr;

            // Default Constructor
            PressureSequence() : Pressure(0.0), InPtr(0)
            {
            }
        };

        struct FluidData
        {
            // Members
            std::string Name;
            bool IsGlycol;

            // Default Constructor
            FluidData() : IsGlycol(false)
            {
            }
        };

        // Object Data
        Array1D<FluidTempData> FluidTemps;
        Array1D<PressureSequence> PressurePtr;
        Array1D<FluidData> FluidNames;

        MaxAlphas = 0;
        MaxNumbers = 0;
        if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FluidProperties:Name") > 0) {
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "FluidProperties:Name", Status, NumAlphas, NumNumbers);
            MaxAlphas = max(MaxAlphas, NumAlphas);
            MaxNumbers = max(MaxNumbers, NumNumbers);
        }
        if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FluidProperties:GlycolConcentration") > 0) {
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
                state, "FluidProperties:GlycolConcentration", Status, NumAlphas, NumNumbers);
            MaxAlphas = max(MaxAlphas, NumAlphas);
            MaxNumbers = max(MaxNumbers, NumNumbers);
        }
        NumOfFluidTempArrays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FluidProperties:Temperatures");
        if (NumOfFluidTempArrays > 0) {
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "FluidProperties:Temperatures", Status, NumAlphas, NumNumbers);
            MaxAlphas = max(MaxAlphas, NumAlphas);
            MaxNumbers = max(MaxNumbers, NumNumbers);
        }
        NumOfSatFluidPropArrays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FluidProperties:Saturated");
        if (NumOfSatFluidPropArrays > 0) {
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "FluidProperties:Saturated", Status, NumAlphas, NumNumbers);
            MaxAlphas = max(MaxAlphas, NumAlphas);
            MaxNumbers = max(MaxNumbers, NumNumbers);
        }
        NumOfSHFluidPropArrays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FluidProperties:Superheated");
        if (NumOfSHFluidPropArrays > 0) {
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "FluidProperties:Superheated", Status, NumAlphas, NumNumbers);
            MaxAlphas = max(MaxAlphas, NumAlphas);
            MaxNumbers = max(MaxNumbers, NumNumbers);
        }
        NumOfGlyFluidPropArrays = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FluidProperties:Concentration");
        if (NumOfGlyFluidPropArrays > 0) {
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "FluidProperties:Concentration", Status, NumAlphas, NumNumbers);
            MaxAlphas = max(MaxAlphas, NumAlphas);
            MaxNumbers = max(MaxNumbers, NumNumbers);
        }

        Alphas.allocate(MaxAlphas);
        cAlphaFieldNames.allocate(MaxAlphas);
        lAlphaFieldBlanks.allocate(MaxAlphas);

        Alphas = "";
        cAlphaFieldNames = "";
        lAlphaFieldBlanks = false;

        Numbers.allocate(MaxNumbers);
        cNumericFieldNames.allocate(MaxNumbers);
        lNumericFieldBlanks.allocate(MaxNumbers);

        Numbers = 0.0;
        cNumericFieldNames = "";
        lNumericFieldBlanks = false;

        InitializeGlycRoutines();

        // Check to see if there is any FluidName input.  If not, this is okay as
        // long as the user only desires to simulate loops with water.  More than
        // one FluidName input is not allowed.
        CurrentModuleObject = "FluidProperties:Name";
        NumOfOptionalInput = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        FluidNames.allocate(NumOfOptionalInput);

        // Get a count on the number of refrigerants and the number of glycols entered
        // so that the main derived types can be allocated
        FluidNum = 0;
        for (Loop = 1; Loop <= NumOfOptionalInput; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Loop,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     Status,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound)) continue;
            ++FluidNum;
            FluidNames(FluidNum).Name = Alphas(1);
            if (UtilityRoutines::SameString(Alphas(2), Refrig)) {
                ++state.dataFluidProps->NumOfRefrigerants;
                FluidNames(FluidNum).IsGlycol = false;
            } else if (UtilityRoutines::SameString(Alphas(2), Glycol)) {
                ++state.dataFluidProps->NumOfGlycols;
                FluidNames(FluidNum).IsGlycol = true;
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid type");
                ShowContinueError(state, "...entered value=\"" + Alphas(2) + ", Only REFRIGERANT or GLYCOL allowed as " + cAlphaFieldNames(2));
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + " Previous errors in input cause program termination.");
        }

        if (state.dataFluidProps->NumOfRefrigerants + 1 > 0) {
            state.dataFluidProps->RefrigData.allocate(state.dataFluidProps->NumOfRefrigerants + 1);
            state.dataFluidProps->RefrigUsed.allocate(state.dataFluidProps->NumOfRefrigerants + 1);
            state.dataFluidProps->RefrigUsed = false;
            state.dataFluidProps->RefrigErrorTracking.allocate(state.dataFluidProps->NumOfRefrigerants + 1);
        }
        if (state.dataFluidProps->NumOfGlycols > 0) {
            state.dataFluidProps->GlyRawData.allocate(state.dataFluidProps->NumOfGlycols);
        }

        // Take the fluid names and assign them to the appropriate derived type
        state.dataFluidProps->NumOfRefrigerants = 1;
        state.dataFluidProps->NumOfGlycols = 0;
        state.dataFluidProps->RefrigData(1).Name = "STEAM";
        state.dataFluidProps->RefrigUsed(1) = true;
        state.dataFluidProps->RefrigErrorTracking(1).Name = "STEAM";
        for (Loop = 1; Loop <= FluidNum; ++Loop) {
            if (!FluidNames(Loop).IsGlycol) {
                ++state.dataFluidProps->NumOfRefrigerants;
                state.dataFluidProps->RefrigData(state.dataFluidProps->NumOfRefrigerants).Name = FluidNames(Loop).Name;
                state.dataFluidProps->RefrigErrorTracking(state.dataFluidProps->NumOfRefrigerants).Name = FluidNames(Loop).Name;
            } else if (FluidNames(Loop).IsGlycol) {
                ++state.dataFluidProps->NumOfGlycols;
                state.dataFluidProps->GlyRawData(state.dataFluidProps->NumOfGlycols).Name = FluidNames(Loop).Name;
            }
        }

        FluidNames.deallocate();

        state.dataFluidProps->RefrigData(1).NumPsPoints = DefaultNumSteamTemps;
        state.dataFluidProps->RefrigData(1).PsTemps.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).PsValues.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).NumHPoints = DefaultNumSteamTemps;
        state.dataFluidProps->RefrigData(1).HTemps.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).HfValues.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).HfgValues.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).NumCpPoints = DefaultNumSteamTemps;
        state.dataFluidProps->RefrigData(1).CpTemps.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).CpfValues.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).CpfgValues.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).NumRhoPoints = DefaultNumSteamTemps;
        state.dataFluidProps->RefrigData(1).RhoTemps.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).RhofValues.allocate(DefaultNumSteamTemps);
        state.dataFluidProps->RefrigData(1).RhofgValues.allocate(DefaultNumSteamTemps);

        state.dataFluidProps->RefrigData(1).PsTemps = DefaultSteamTemps;
        state.dataFluidProps->RefrigData(1).PsValues = DefaultSteamPressData;
        state.dataFluidProps->RefrigData(1).HTemps = DefaultSteamTemps;
        state.dataFluidProps->RefrigData(1).HfValues = DefaultSteamEnthalpyFluidData;
        state.dataFluidProps->RefrigData(1).HfgValues = DefaultSteamEnthalpyGasFluidData;
        state.dataFluidProps->RefrigData(1).CpTemps = DefaultSteamTemps;
        state.dataFluidProps->RefrigData(1).CpfValues = DefaultSteamCpFluidData;
        state.dataFluidProps->RefrigData(1).CpfgValues = DefaultSteamCpGasFluidData;
        state.dataFluidProps->RefrigData(1).RhoTemps = DefaultSteamTemps;
        state.dataFluidProps->RefrigData(1).RhofValues = DefaultSteamDensityFluidData;
        state.dataFluidProps->RefrigData(1).RhofgValues = DefaultSteamDensityGasFluidData;

        state.dataFluidProps->RefrigData(1).NumSuperTempPts = DefaultNumSteamSuperheatedTemps;
        state.dataFluidProps->RefrigData(1).NumSuperPressPts = DefaultNumSteamSuperheatedPressure;
        state.dataFluidProps->RefrigData(1).SHTemps.allocate(state.dataFluidProps->RefrigData(1).NumSuperTempPts);
        state.dataFluidProps->RefrigData(1).SHPress.allocate(state.dataFluidProps->RefrigData(1).NumSuperPressPts);
        state.dataFluidProps->RefrigData(1).HshValues.allocate(state.dataFluidProps->RefrigData(1).NumSuperPressPts,
                                                               state.dataFluidProps->RefrigData(1).NumSuperTempPts);
        state.dataFluidProps->RefrigData(1).RhoshValues.allocate(state.dataFluidProps->RefrigData(1).NumSuperPressPts,
                                                                 state.dataFluidProps->RefrigData(1).NumSuperTempPts);
        state.dataFluidProps->RefrigData(1).SHTemps = DefaultSteamSuperheatedTemps;
        state.dataFluidProps->RefrigData(1).SHPress = DefaultSteamSuperheatedPressData;
        state.dataFluidProps->RefrigData(1).HshValues = DefaultSteamSuperheatedEnthalpyData;
        state.dataFluidProps->RefrigData(1).RhoshValues = DefaultSteamSuperheatedDensityData;

        // Read in all of the temperature arrays in the input file
        FluidTemps.allocate(NumOfFluidTempArrays);

        CurrentModuleObject = "FluidProperties:Temperatures";

        for (Loop = 1; Loop <= NumOfFluidTempArrays; ++Loop) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Loop,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     Status,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            FluidTemps(Loop).Name = Alphas(1);
            FluidTemps(Loop).NumOfTemps = NumNumbers;

            FluidTemps(Loop).Temps.allocate(FluidTemps(Loop).NumOfTemps);
            FluidTemps(Loop).Temps = Numbers({1, NumNumbers});

            for (TempLoop = 2; TempLoop <= FluidTemps(Loop).NumOfTemps; ++TempLoop) {
                if (FluidTemps(Loop).Temps(TempLoop) <= FluidTemps(Loop).Temps(TempLoop - 1)) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + " name=" + FluidTemps(Loop).Name +
                                        ", lists must have data in ascending order");
                    ShowContinueError(state,
                                      format("First out of order occurrence at Temperature #({}) {{{:.R3}}} >= Temp({}) {{{:.R3}}}",
                                             TempLoop - 1,
                                             FluidTemps(Loop).Temps(TempLoop - 1),
                                             TempLoop,
                                             FluidTemps(Loop).Temps(TempLoop)));
                    ErrorsFound = true;
                    break;
                }
            }
        }

        // *************** REFRIGERANTS ***************
        // Go through each refrigerant found in the fluid names statement and read in the data
        // Note that every valid fluid must have ALL of the necessary data or a fatal error will
        // be produced.
        for (Loop = 2; Loop <= state.dataFluidProps->NumOfRefrigerants; ++Loop) {

            // For each property, cycle through all the valid input until the proper match is found.

            // **********    SATURATED DATA SECTION    **********

            // Get: ***** Saturation Pressure temperatures and data (fluidgas only) *****
            // This section added by S.J.Rees May 2002.
            CurrentModuleObject = "FluidProperties:Saturated";
            TempsName = "";
            for (InData = 1; InData <= NumOfSatFluidPropArrays; ++InData) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Pressure)) && (UtilityRoutines::SameString(Alphas(3), GasFluid))) {

                    for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {

                        if (UtilityRoutines::SameString(Alphas(4), FluidTemps(TempLoop).Name)) {
                            TempsName = FluidTemps(TempLoop).Name;
                            // At this point, we have found the correct input line and found a match
                            // for the temperature array.  It's time to load up the local derived type.
                            state.dataFluidProps->RefrigData(Loop).NumPsPoints = FluidTemps(TempLoop).NumOfTemps;
                            state.dataFluidProps->RefrigData(Loop).PsTemps.allocate(state.dataFluidProps->RefrigData(Loop).NumPsPoints);
                            state.dataFluidProps->RefrigData(Loop).PsValues.allocate(state.dataFluidProps->RefrigData(Loop).NumPsPoints);

                            // Make sure the number of points in the two arrays (temps and values) are the same
                            if (NumNumbers != state.dataFluidProps->RefrigData(Loop).NumPsPoints) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowContinueError(state,
                                                  "Temperature Name=" + TempsName +
                                                      ", Temperature array and fluid saturation pressure array must have the same number of points");
                                ShowContinueError(state,
                                                  format("Temperature # points={} whereas {} # pressure points={}",
                                                         NumNumbers,
                                                         state.dataFluidProps->RefrigData(Loop).Name,
                                                         state.dataFluidProps->RefrigData(Loop).NumPsPoints));
                                ErrorsFound = true;
                                break; // the TempLoop DO Loop
                            }

                            // Same number of points so assign the values
                            state.dataFluidProps->RefrigData(Loop).PsTemps = FluidTemps(TempLoop).Temps;
                            state.dataFluidProps->RefrigData(Loop).PsValues = Numbers({1, NumNumbers});

                            break; // the TempLoop DO loop
                        }

                        // If it made it all the way to the last temperature array and didn't find a match, then no match was found
                        if (TempLoop == NumOfFluidTempArrays) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state, "Found saturated fluid gas/fluid pressure input but no matching temperature array");
                            ShowContinueError(state, "Entered Temperature Name=" + TempsName);
                            ErrorsFound = true;
                        }

                    } // ...end of FluidTemps DO loop

                    break; // the InData DO loop
                }

                // If it made it all the way to the last input occurrence and didn't find a match,
                // then no sat press data found
                if (InData == NumOfSatFluidPropArrays) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state,
                                      "No Gas/Fluid Saturation Pressure found. Need properties with " + cAlphaFieldNames(2) + "=\"Pressure\" and " +
                                          cAlphaFieldNames(3) + "=\"FluidGas\".");
                    ErrorsFound = true;
                }

            } // ...end of DO loop through all of the input syntax trying to find saturation pressure for this refrigerant

            // Get: ***** ENTHALPY of SATURATED LIQUID *****
            CurrentModuleObject = "FluidProperties:Saturated";
            TempsName = "";
            for (InData = 1; InData <= NumOfSatFluidPropArrays; ++InData) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Enthalpy)) && (UtilityRoutines::SameString(Alphas(3), Fluid))) {

                    for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {

                        if (UtilityRoutines::SameString(Alphas(4), FluidTemps(TempLoop).Name)) {
                            TempsName = FluidTemps(TempLoop).Name;
                            // At this point, we have found the correct input line and found a match
                            // for the temperature array.  It's time to load up the local derived type.
                            state.dataFluidProps->RefrigData(Loop).NumHPoints = FluidTemps(TempLoop).NumOfTemps;
                            state.dataFluidProps->RefrigData(Loop).HTemps.allocate(state.dataFluidProps->RefrigData(Loop).NumHPoints);
                            state.dataFluidProps->RefrigData(Loop).HfValues.allocate(state.dataFluidProps->RefrigData(Loop).NumHPoints);

                            // Make sure the number of points in the two arrays (temps and values) are the same
                            if (NumNumbers != state.dataFluidProps->RefrigData(Loop).NumHPoints) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowSevereError(state,
                                                "Temperature Name=" + TempsName +
                                                    ", Temperature array and saturated fluid enthalpy array must have the same number of points");
                                ShowContinueError(state,
                                                  format("Temperature # points={} whereas {} # points={}",
                                                         NumNumbers,
                                                         state.dataFluidProps->RefrigData(Loop).Name,
                                                         state.dataFluidProps->RefrigData(Loop).NumHPoints));
                                ErrorsFound = true;
                                break; // the TempLoop DO Loop
                            }

                            // Same number of points so assign the values
                            state.dataFluidProps->RefrigData(Loop).HTemps = FluidTemps(TempLoop).Temps;
                            state.dataFluidProps->RefrigData(Loop).HfValues = Numbers({1, NumNumbers});

                            break; // the TempLoop DO loop
                        }

                        // If it made it all the way to the last temperature array and didn't find a match, then no match was found
                        if (TempLoop == NumOfFluidTempArrays) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state, "Found saturated fluid enthalpy input but no matching temperature array");
                            ShowContinueError(state, "Entered Temperature Name=" + TempsName);
                            ErrorsFound = true;
                        }

                    } // ...end of FluidTemps DO loop

                    break; // the InData DO loop
                }

                // If it made it all the way to the last input occurrence and didn't find a match, then no sat fluid enthalpy data found
                if (InData == NumOfSatFluidPropArrays) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state,
                                      "No Saturated Fluid Enthalpy found. Need properties to be entered with " + cAlphaFieldNames(2) +
                                          "=\"Enthalpy\" and " + cAlphaFieldNames(3) + "=\"Fluid\".");
                    ErrorsFound = true;
                }

            } // ...end of DO loop through all of the input syntax trying to find saturated fluid enthalpy for this refrigerant

            // Get: ***** ENTHALPY of SATURATED LIQUID/VAPOR ***** (difference between Hf and Hg, i.e. Hfg)
            CurrentModuleObject = "FluidProperties:Saturated";
            for (InData = 1; InData <= NumOfSatFluidPropArrays; ++InData) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Enthalpy)) && (UtilityRoutines::SameString(Alphas(3), GasFluid))) {

                    for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {

                        if (UtilityRoutines::SameString(Alphas(4), FluidTemps(TempLoop).Name)) {
                            if (!UtilityRoutines::SameString(FluidTemps(TempLoop).Name, TempsName)) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowContinueError(state, "Temperatures for enthalpy fluid and gas/fluid points are not the same");
                                ShowContinueError(state, "Name=" + Alphas(4) + " => " + FluidTemps(TempLoop).Name + " /= " + TempsName);
                                ErrorsFound = true;
                                break;
                            }
                            // At this point, we have found the correct input line and found a match
                            // for the temperature array.  It's time to load up the local derived type.
                            state.dataFluidProps->RefrigData(Loop).HfgValues.allocate(state.dataFluidProps->RefrigData(Loop).NumHPoints);

                            // Make sure the number of points in the two arrays (temps and values) are the same
                            if (NumNumbers != state.dataFluidProps->RefrigData(Loop).NumHPoints) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowContinueError(
                                    state,
                                    "Temperature Name=" + TempsName +
                                        ", Temperature array and saturated gas/fluid enthalpy array must have the same number of points");
                                ShowContinueError(state,
                                                  format("Temperature # points={} whereas {} # points={}",
                                                         NumNumbers,
                                                         state.dataFluidProps->RefrigData(Loop).Name,
                                                         state.dataFluidProps->RefrigData(Loop).NumHPoints));
                                ErrorsFound = true;
                                break; // the TempLoop DO Loop
                            }

                            // Same number of points so assign the values
                            state.dataFluidProps->RefrigData(Loop).HfgValues = Numbers({1, NumNumbers});

                            break; // the TempLoop DO loop
                        }

                        // If it made it all the way to the last temperature array and didn't find a match, then no match was found
                        if (TempLoop == NumOfFluidTempArrays) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state, "Found saturated gas/fluid enthalpy input but no matching temperature array");
                            ShowContinueError(state, "Entered Temperature Name=" + TempsName);
                            ErrorsFound = true;
                        }

                    } // ...end of FluidTemps DO loop

                    break; // the InData DO loop
                }

                // If it made it all the way to the last input occurrence and didn't find a match, then no sat f/g enthalpy data found
                if (InData == NumOfSatFluidPropArrays) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state,
                                      "No Saturated Gas/Fluid Enthalpy found. Need properties to be entered with " + cAlphaFieldNames(2) +
                                          "=\"Enthalpy\" and " + cAlphaFieldNames(3) + "=\"FluidGas\".");
                    ErrorsFound = true;
                }

            } // ...end of DO loop through all of the input syntax trying to find saturated gas/fluid enthalpy for this refrigerant

            // Get: ***** SPECIFIC HEAT of SATURATED LIQUID *****
            CurrentModuleObject = "FluidProperties:Saturated";
            TempsName = "";
            for (InData = 1; InData <= NumOfSatFluidPropArrays; ++InData) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), SpecificHeat)) && (UtilityRoutines::SameString(Alphas(3), Fluid))) {

                    for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {

                        if (UtilityRoutines::SameString(Alphas(4), FluidTemps(TempLoop).Name)) {
                            TempsName = FluidTemps(TempLoop).Name;
                            // At this point, we have found the correct input line and found a match
                            // for the temperature array.  It's time to load up the local derived type.
                            state.dataFluidProps->RefrigData(Loop).NumCpPoints = FluidTemps(TempLoop).NumOfTemps;
                            state.dataFluidProps->RefrigData(Loop).CpTemps.allocate(state.dataFluidProps->RefrigData(Loop).NumCpPoints);
                            state.dataFluidProps->RefrigData(Loop).CpfValues.allocate(state.dataFluidProps->RefrigData(Loop).NumCpPoints);

                            // Make sure the number of points in the two arrays (temps and values) are the same
                            if (NumNumbers != state.dataFluidProps->RefrigData(Loop).NumCpPoints) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowSevereError(state,
                                                "Temperature Name=" + TempsName +
                                                    ", Temperature array and saturated fluid Cp array must have the same number of points");
                                ShowContinueError(state,
                                                  format("Temperature # points={} whereas {} # Cp points={}",
                                                         NumNumbers,
                                                         state.dataFluidProps->RefrigData(Loop).Name,
                                                         state.dataFluidProps->RefrigData(Loop).NumCpPoints));
                                ErrorsFound = true;
                                break; // the TempLoop DO Loop
                            }

                            // Same number of points so assign the values
                            state.dataFluidProps->RefrigData(Loop).CpTemps = FluidTemps(TempLoop).Temps;
                            state.dataFluidProps->RefrigData(Loop).CpfValues = Numbers({1, NumNumbers});

                            break; // the TempLoop DO loop
                        }

                        // If it made it all the way to the last temperature array and didn't find a match, then no match was found
                        if (TempLoop == NumOfFluidTempArrays) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state, "Found saturated fluid specific heat (Cp) input but no matching temperature array");
                            ShowContinueError(state, "Entered Temperature Name=" + TempsName);
                            ErrorsFound = true;
                        }

                    } // ...end of FluidTemps DO loop

                    break; // the InData DO loop
                }

                // If it made it all the way to the last input occurrence and didn't find a match, then no sat fluid Cp data found
                if (InData == NumOfSatFluidPropArrays) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state,
                                      "No Saturated Fluid Specific Heat found. Need properties to be entered with " + cAlphaFieldNames(2) +
                                          "=\"SpecificHeat\" and " + cAlphaFieldNames(3) + "=\"Fluid\".");
                    ErrorsFound = true;
                }

            } // ...end of DO loop through all of the input syntax trying to find saturated fluid Cp for this refrigerant

            // Get: ***** SPECIFIC HEAT of SATURATED LIQUID/VAPOR ***** (difference between Cpf and Cpg, i.e. Cpfg)
            CurrentModuleObject = "FluidProperties:Saturated";
            for (InData = 1; InData <= NumOfSatFluidPropArrays; ++InData) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), SpecificHeat)) && (UtilityRoutines::SameString(Alphas(3), GasFluid))) {

                    for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {

                        if (UtilityRoutines::SameString(Alphas(4), FluidTemps(TempLoop).Name)) {
                            if (!UtilityRoutines::SameString(FluidTemps(TempLoop).Name, TempsName)) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowContinueError(state, "Temperatures for specific heat fluid and gas/fluid points are not the same");
                                ShowContinueError(state, "Name=" + Alphas(4) + " => " + FluidTemps(TempLoop).Name + " /= " + TempsName);
                                ErrorsFound = true;
                                break;
                            }
                            // At this point, we have found the correct input line and found a match
                            // for the temperature array.  It's time to load up the local derived type.
                            state.dataFluidProps->RefrigData(Loop).CpfgValues.allocate(state.dataFluidProps->RefrigData(Loop).NumCpPoints);

                            // Make sure the number of points in the two arrays (temps and values) are the same
                            if (NumNumbers != state.dataFluidProps->RefrigData(Loop).NumCpPoints) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowContinueError(state,
                                                  "Temperature Name=" + TempsName +
                                                      ", Temperature array and saturated gas/fluid Cp array must have the same number of points");
                                ShowContinueError(state,
                                                  format("Temperature # points={} whereas {} # Cp points={}",
                                                         NumNumbers,
                                                         state.dataFluidProps->RefrigData(Loop).Name,
                                                         state.dataFluidProps->RefrigData(Loop).NumCpPoints));
                                ErrorsFound = true;
                                break; // the TempLoop DO Loop
                            }

                            // Same number of points so assign the values
                            state.dataFluidProps->RefrigData(Loop).CpfgValues = Numbers({1, NumNumbers});

                            break; // the TempLoop DO loop
                        }

                        // If it made it all the way to the last temperature array and didn't find a match, then no match was found
                        if (TempLoop == NumOfFluidTempArrays) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state, "Found saturated gas/fluid specific heat (Cp) input but no matching temperature array");
                            ShowContinueError(state, "Entered Temperature Name=" + TempsName);
                            ErrorsFound = true;
                        }

                    } // ...end of FluidTemps DO loop

                    break; // the InData DO loop
                }

                // If it made it all the way to the last input occurrence and didn't find a match, then no sat f/g Cp data found
                if (InData == NumOfSatFluidPropArrays) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state,
                                      "No Saturated Gas/Fluid Specific Heat found. Need properties to be entered with " + cAlphaFieldNames(2) +
                                          "=\"SpecificHeat\" and " + cAlphaFieldNames(3) + "=\"FluidGas\".");
                    ErrorsFound = true;
                }

            } // ...end of DO loop through all of the input syntax trying to find saturated gas/fluid Cp for this refrigerant

            // Get: ***** DENSITY of SATURATED LIQUID *****
            CurrentModuleObject = "FluidProperties:Saturated";
            TempsName = "";
            for (InData = 1; InData <= NumOfSatFluidPropArrays; ++InData) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Density)) && (UtilityRoutines::SameString(Alphas(3), Fluid))) {

                    for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {

                        if (UtilityRoutines::SameString(Alphas(4), FluidTemps(TempLoop).Name)) {
                            TempsName = FluidTemps(TempLoop).Name;
                            // At this point, we have found the correct input line and found a match
                            // for the temperature array.  It's time to load up the local derived type.
                            state.dataFluidProps->RefrigData(Loop).NumRhoPoints = FluidTemps(TempLoop).NumOfTemps;
                            state.dataFluidProps->RefrigData(Loop).RhoTemps.allocate(state.dataFluidProps->RefrigData(Loop).NumRhoPoints);
                            state.dataFluidProps->RefrigData(Loop).RhofValues.allocate(state.dataFluidProps->RefrigData(Loop).NumRhoPoints);

                            // Make sure the number of points in the two arrays (temps and values) are the same
                            if (NumNumbers != state.dataFluidProps->RefrigData(Loop).NumRhoPoints) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowContinueError(state,
                                                  "Temperature Name=" + TempsName +
                                                      ", Temperature array and saturated fluid density array must have the same number of points");
                                ShowContinueError(state,
                                                  format("Temperature # points={} whereas {} # Density points={}",
                                                         NumNumbers,
                                                         state.dataFluidProps->RefrigData(Loop).Name,
                                                         state.dataFluidProps->RefrigData(Loop).NumRhoPoints));
                                ErrorsFound = true;
                                break; // the TempLoop DO Loop
                            }

                            // Same number of points so assign the values
                            state.dataFluidProps->RefrigData(Loop).RhoTemps = FluidTemps(TempLoop).Temps;
                            state.dataFluidProps->RefrigData(Loop).RhofValues = Numbers({1, NumNumbers});

                            break; // the TempLoop DO loop
                        }

                        // If it made it all the way to the last temperature array and didn't find a match, then no match was found
                        if (TempLoop == NumOfFluidTempArrays) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state, "Found saturated fluid density input but no matching temperature array");
                            ShowContinueError(state, "Entered Temperature Name=" + TempsName);
                            ErrorsFound = true;
                        }

                    } // ...end of FluidTemps DO loop

                    break; // the InData DO loop
                }

                // If it made it all the way to the last input occurrence and didn't find a match, then no sat fluid density data found
                if (InData == NumOfSatFluidPropArrays) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state,
                                      "No Saturated Fluid Density found. Need properties to be entered with " + cAlphaFieldNames(2) +
                                          "=\"Density\" and " + cAlphaFieldNames(3) + "=\"Fluid\".");
                    ErrorsFound = true;
                }

            } // ...end of DO loop through all of the input syntax trying to find saturated fluid enthalpy for this refrigerant

            // Get: ***** DENSITY of SATURATED LIQUID/VAPOR ***** (difference between Rhof and Rhog, i.e. Rhofg)
            CurrentModuleObject = "FluidProperties:Saturated";
            for (InData = 1; InData <= NumOfSatFluidPropArrays; ++InData) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Density)) && (UtilityRoutines::SameString(Alphas(3), GasFluid))) {

                    for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {

                        if (UtilityRoutines::SameString(Alphas(4), FluidTemps(TempLoop).Name)) {
                            if (!UtilityRoutines::SameString(FluidTemps(TempLoop).Name, TempsName)) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowContinueError(state, "Temperatures for density fluid and gas/fluid points are not the same");
                                ShowContinueError(state, "Name=" + Alphas(4) + " => " + FluidTemps(TempLoop).Name + " /= " + TempsName);
                                ErrorsFound = true;
                                break;
                            }
                            // At this point, we have found the correct input line and found a match
                            // for the temperature array.  It's time to load up the local derived type.
                            state.dataFluidProps->RefrigData(Loop).RhofgValues.allocate(state.dataFluidProps->RefrigData(Loop).NumRhoPoints);

                            // Make sure the number of points in the two arrays (temps and values) are the same
                            if (NumNumbers != state.dataFluidProps->RefrigData(Loop).NumRhoPoints) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                                ShowContinueError(
                                    state,
                                    "Temperature Name=" + TempsName +
                                        ", Temperature array and saturated gas/fluid density array must have the same number of points");
                                ShowContinueError(state,
                                                  format("Temperature # points={} whereas {} # density points={}",
                                                         NumNumbers,
                                                         state.dataFluidProps->RefrigData(Loop).Name,
                                                         state.dataFluidProps->RefrigData(Loop).NumRhoPoints));
                                ErrorsFound = true;
                                break; // the TempLoop DO Loop
                            }

                            // Same number of points so assign the values
                            state.dataFluidProps->RefrigData(Loop).RhofgValues = Numbers({1, NumNumbers});

                            break; // the TempLoop DO loop
                        }

                        // If it made it all the way to the last temperature array and didn't find a match, then no match was found
                        if (TempLoop == NumOfFluidTempArrays) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state, "Found saturated gas/fluid density input but no matching temperature array");
                            ShowContinueError(state, "Entered Temperature Name=" + TempsName);
                            ErrorsFound = true;
                        }

                    } // ...end of FluidTemps DO loop

                    break; // the InData DO loop
                }

                // If it made it all the way to the last input occurrence and didn't find a match, then no sat f/g density data found
                if (InData == NumOfSatFluidPropArrays) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowSevereError(state,
                                    "No Saturated Gas/Fluid Density found. Need properties to be entered with " + cAlphaFieldNames(2) +
                                        "=\"Density\" and " + cAlphaFieldNames(3) + "=\"FluidGas\".");
                    ErrorsFound = true;
                }

            } // ...end of DO loop through all of the input syntax trying to find saturated gas/fluid density for this refrigerant

            // Check: TEMPERATURES for saturated density (must all be the same)
            //    IF (RefrigData(Loop)%NumCpPoints /= RefrigData(Loop)%NumCpPoints) THEN
            //!!!  Error -- can never happen, does this mean NumCp vs. NumRho?
            //      CALL ShowFatalError(state, 'GetFluidPropertiesData: Number of specific heat fluid and gas/fluid points are not the same')
            //    ELSE
            //      DO TempLoop = 1, RefrigData(Loop)%NumCpPoints
            //!!! Error -- something else that can never happen
            //        IF (ABS(RefrigData(Loop)%CpTemps(TempLoop)-RefrigData(Loop)%CpTemps(TempLoop)) > TempToler) THEN
            //          CALL ShowSevereError(state, 'GetFluidPropertiesData: Temperatures for specific heat fluid and '// &
            //                               'gas/fluid points are not the same')
            //          CALL ShowContinueError(state, 'Error occurs in Refrigerant Data Name='//TRIM(RefrigData(Loop)%Name))
            //          WRITE(String1,*) TempLoop
            //          String1=ADJUSTL(String1)
            //          String2=TrimSigDigits(RefrigData(Loop)%CpTemps(TempLoop),3)
            //          String2=ADJUSTL(String2)
            //          String4=TrimSigDigits(RefrigData(Loop)%CpTemps(TempLoop),3)
            //          String4=ADJUSTL(String4)
            //          CALL ShowContinueError(state, 'First Occurrence at CpTemp('//TRIM(String1)//') {'//TRIM(String2)//'} /=
            //          {'//TRIM(String4)//'}') ErrorsFound=.TRUE. EXIT
            //        ENDIF
            //      END DO
            //    END IF

            //   Error check on entering saturated data
            iTemp = 0;
            CurrentModuleObject = "FluidProperties:Saturated";
            for (InData = 1; InData <= NumOfSatFluidPropArrays; ++InData) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                if (UtilityRoutines::SameString(Alphas(3), Fluid)) {
                    if (!UtilityRoutines::SameString(Alphas(2), Enthalpy) && !UtilityRoutines::SameString(Alphas(2), SpecificHeat) &&
                        !UtilityRoutines::SameString(Alphas(2), Density)) {
                        if (iTemp == 0) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state,
                                              cAlphaFieldNames(3) + "=\"" + std::string{Fluid} + "\", but " + cAlphaFieldNames(2) + "=\"" +
                                                  Alphas(2) + "\" is not valid.");
                            ShowContinueError(state,
                                              "Valid choices are \"" + std::string{Enthalpy} + "\", \"" + std::string{SpecificHeat} + "\", \"" +
                                                  std::string{Density} + "\".");
                            ShowContinueError(state, "This fluid property will not be processed mor available for the simulation.");
                        }
                        ++iTemp;
                    }
                } else if (UtilityRoutines::SameString(Alphas(3), GasFluid)) {
                    if (!UtilityRoutines::SameString(Alphas(2), Pressure) && !UtilityRoutines::SameString(Alphas(2), Enthalpy) &&
                        !UtilityRoutines::SameString(Alphas(2), SpecificHeat) && !UtilityRoutines::SameString(Alphas(2), Density)) {
                        if (iTemp == 0) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state,
                                              cAlphaFieldNames(3) + "=\"" + std::string{Fluid} + "\", but " + cAlphaFieldNames(2) + "=\"" +
                                                  Alphas(2) + "\" is not valid.");
                            ShowContinueError(state,
                                              "Valid choices are \"" + std::string{Pressure} + "\", \"" + std::string{Enthalpy} + "\", \"" +
                                                  std::string{SpecificHeat} + "\", \"" + std::string{Density} + "\".");
                            ShowContinueError(state, "This fluid property will not be processed nor available for the simulation.");
                        }
                        ++iTemp;
                    }
                } else {
                    if (iTemp == 0) {
                        ShowWarningError(state,
                                         std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                        ShowContinueError(state, cAlphaFieldNames(3) + "=\"" + Alphas(3) + "\" is not valid.");
                        ShowContinueError(state, "Valid choices are \"" + std::string{Fluid} + "\", \"" + std::string{GasFluid} + "\".");
                        ShowContinueError(state, "This fluid property will not be processed nor available for the simulation.");
                    }
                    ++iTemp;
                }
            }

            if (iTemp > 1) {
                ShowWarningError(state, format("{}{} has {} similar errors to the previous.", RoutineName, CurrentModuleObject, iTemp - 1));
            }

            // **********   SUPERHEATED DATA SECTION   **********
            // Get: ***** ENTHALPY of SUPERHEATED GAS  *****
            // First find the number of pressure value syntax lines have been entered and
            // make sure that all of the pressure input is linked to the same temperature list
            CurrentModuleObject = "FluidProperties:Superheated";
            TempsName = "";
            FirstSHMatch = true;
            NumOfPressPts = 0;
            for (InData = 1; InData <= NumOfSHFluidPropArrays; ++InData) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Enthalpy))) {
                    ++NumOfPressPts;
                    if (FirstSHMatch) {
                        TempsName = Alphas(3);
                        FirstSHMatch = false;
                    } else {
                        if (!UtilityRoutines::SameString(TempsName, Alphas(3))) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                            ShowContinueError(state, "All superheated data for the same property must use the same temperature list");
                            ShowContinueError(state, "Expected name=" + TempsName + ", Entered name=" + Alphas(3));
                            ErrorsFound = true;
                        }
                    }
                }
            }
            if (NumOfPressPts == 0) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                ShowContinueError(state, "No pressure data found for superheated enthalpy");
                ErrorsFound = true;
            }

            // Now allocate the arrays and read the data into the proper place
            // First, allocate the temperature array and transfer the data from the FluidTemp array
            for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {
                if (UtilityRoutines::SameString(TempsName, FluidTemps(TempLoop).Name)) {
                    state.dataFluidProps->RefrigData(Loop).NumSuperTempPts = FluidTemps(TempLoop).NumOfTemps;
                    state.dataFluidProps->RefrigData(Loop).SHTemps.allocate(state.dataFluidProps->RefrigData(Loop).NumSuperTempPts);
                    state.dataFluidProps->RefrigData(Loop).SHTemps = FluidTemps(TempLoop).Temps;
                    break; // the TempLoop DO loop
                }
                if (TempLoop == NumOfFluidTempArrays) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state, "No match for temperature array name found with superheated enthalpy data");
                    ShowContinueError(state, "Entered Temperature Name=" + TempsName);
                    ErrorsFound = true;
                }
            }

            // Next, allocate the pressure related arrays
            state.dataFluidProps->RefrigData(Loop).NumSuperPressPts = NumOfPressPts;
            state.dataFluidProps->RefrigData(Loop).SHPress.allocate(state.dataFluidProps->RefrigData(Loop).NumSuperPressPts);
            state.dataFluidProps->RefrigData(Loop).HshValues.allocate(state.dataFluidProps->RefrigData(Loop).NumSuperPressPts,
                                                                      state.dataFluidProps->RefrigData(Loop).NumSuperTempPts);

            // Finally, get the pressure and enthalpy values from the user input
            CurrentModuleObject = "FluidProperties:Superheated";
            NumOfPressPts = 0;
            PressurePtr.allocate(NumOfSHFluidPropArrays);
            for (InData = 1; InData <= NumOfSHFluidPropArrays; ++InData) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);

                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Enthalpy))) {
                    ++NumOfPressPts;
                    if (Numbers(1) <= 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                        ShowContinueError(state, format("Negative pressures not allowed in fluid property input data, Value =[{:.3R}].", Numbers(1)));
                        ErrorsFound = true;
                    }
                    PressurePtr(NumOfPressPts).Pressure = Numbers(1);
                    PressurePtr(NumOfPressPts).InPtr = InData;
                }
            }

            // Sort Pressure list
            // insertionSort
            for (InData = 2; InData <= NumOfPressPts; ++InData) {
                pTemp = PressurePtr(InData).Pressure;
                iTemp = PressurePtr(InData).InPtr;
                j = InData - 1;
                while (j >= 1 && PressurePtr(j).Pressure > pTemp) {
                    PressurePtr(j + 1).Pressure = PressurePtr(j).Pressure;
                    PressurePtr(j + 1).InPtr = PressurePtr(j).InPtr;
                    --j;
                    if (j == 0) break;
                }
                PressurePtr(j + 1).Pressure = pTemp;
                PressurePtr(j + 1).InPtr = iTemp;
            }

            for (InData = 1; InData <= NumOfPressPts; ++InData) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         PressurePtr(InData).InPtr,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                state.dataFluidProps->RefrigData(Loop).SHPress(InData) = Numbers(1);
                // a little error trapping
                if (InData > 1) {
                    if (state.dataFluidProps->RefrigData(Loop).SHPress(InData) <= state.dataFluidProps->RefrigData(Loop).SHPress(InData - 1)) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                        ShowContinueError(state, "Pressures must be entered in ascending order for fluid property data");
                        ShowContinueError(state,
                                          format("First Occurrence at Pressure({}) {{{:.3R}}} >= Pressure({}) {{{:.3R}}}",
                                                 InData - 1,
                                                 state.dataFluidProps->RefrigData(Loop).SHPress(InData - 1),
                                                 InData,
                                                 state.dataFluidProps->RefrigData(Loop).SHPress(InData)));
                        ErrorsFound = true;
                        break;
                    }
                }
                if ((NumNumbers - 1) == state.dataFluidProps->RefrigData(Loop).NumSuperTempPts) {
                    state.dataFluidProps->RefrigData(Loop).HshValues(InData, {1, state.dataFluidProps->RefrigData(Loop).NumSuperTempPts}) =
                        Numbers({2, NumNumbers});
                } else {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state, "Number of superheated enthalpy data points not equal to number of temperature points");
                    ErrorsFound = true;
                }
            }

            PressurePtr.deallocate();

            // Get: ***** DENSITY of SUPERHEATED GAS  *****
            // First find the number of pressure value syntax lines have been entered and
            // make sure that all of the pressure input is linked to the same temperature list
            // Then allocate the arrays and read the data into the proper place
            state.dataFluidProps->RefrigData(Loop).RhoshValues.allocate(state.dataFluidProps->RefrigData(Loop).NumSuperPressPts,
                                                                        state.dataFluidProps->RefrigData(Loop).NumSuperTempPts);
            CurrentModuleObject = "FluidProperties:Superheated";
            NumOfPressPts = 0;
            PressurePtr.allocate(NumOfSHFluidPropArrays);
            for (InData = 1; InData <= NumOfSHFluidPropArrays; ++InData) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->RefrigData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Density))) {
                    ++NumOfPressPts;
                    if (Numbers(1) <= 0.0) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                        ShowContinueError(state, format("Negative pressures not allowed in fluid property input data, Value =[{:.3R}].", Numbers(1)));
                        ErrorsFound = true;
                    }
                    PressurePtr(NumOfPressPts).Pressure = Numbers(1);
                    PressurePtr(NumOfPressPts).InPtr = InData;
                }
            }

            // Sort Pressure list
            // insertionSort
            for (InData = 2; InData <= NumOfPressPts; ++InData) {
                pTemp = PressurePtr(InData).Pressure;
                iTemp = PressurePtr(InData).InPtr;
                j = InData - 1;
                while (j >= 1 && PressurePtr(j).Pressure > pTemp) {
                    PressurePtr(j + 1).Pressure = PressurePtr(j).Pressure;
                    PressurePtr(j + 1).InPtr = PressurePtr(j).InPtr;
                    --j;
                    if (j == 0) break;
                }
                PressurePtr(j + 1).Pressure = pTemp;
                PressurePtr(j + 1).InPtr = iTemp;
            }

            for (InData = 1; InData <= NumOfPressPts; ++InData) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         PressurePtr(InData).InPtr,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                if (std::abs(Numbers(1) - state.dataFluidProps->RefrigData(Loop).SHPress(InData)) > PressToler) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state, "All superheated data for the same refrigerant must use the same pressure data");
                    ErrorsFound = true;
                }
                if (!UtilityRoutines::SameString(TempsName, Alphas(3))) {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state, "All superheated data for the same property must use the same temperature list");
                    ErrorsFound = true;
                }
                if ((NumNumbers - 1) == state.dataFluidProps->RefrigData(Loop).NumSuperTempPts) {
                    state.dataFluidProps->RefrigData(Loop).RhoshValues(InData, {1, state.dataFluidProps->RefrigData(Loop).NumSuperTempPts}) =
                        Numbers({2, NumNumbers});
                } else {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                    ShowContinueError(state, "Number of superheated density data points not equal to number of temperature points");
                    ErrorsFound = true;
                }
            }

            PressurePtr.deallocate();

            //   Error check on entering superheated data
            iTemp = 0;
            CurrentModuleObject = "FluidProperties:Superheated";
            for (InData = 1; InData <= NumOfSHFluidPropArrays; ++InData) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                if (!UtilityRoutines::SameString(Alphas(2), Enthalpy) && !UtilityRoutines::SameString(Alphas(2), Density)) {
                    if (iTemp == 0) {
                        ShowWarningError(state,
                                         std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                        ShowContinueError(state, cAlphaFieldNames(2) + "=\"" + Alphas(2) + "\" is not valid.");
                        ShowContinueError(state, "Valid choices are \"" + std::string{Enthalpy} + "\", \"" + std::string{Density} + "\".");
                        ShowContinueError(state, format("Pressure value of this item=[{:.2R}].", Numbers(1)));
                        ShowContinueError(state, "This fluid property will not be processed nor available for the simulation.");
                    }
                    ++iTemp;
                }
            }

            if (iTemp > 1) {
                ShowWarningError(state, format("{}{} has {} similar errors to the previous.", RoutineName, CurrentModuleObject, iTemp - 1));
            }

            if (NumOfPressPts == 0) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                ShowSevereError(state, "No pressure data found for superheated density");
                ErrorsFound = true;
            }
            if (NumOfPressPts != state.dataFluidProps->RefrigData(Loop).NumSuperPressPts) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->RefrigData(Loop).Name);
                ShowSevereError(state, "Number of pressure points for superheated data different for enthalpy and density");
                ErrorsFound = true;
            }

        } // ...end of DO loop through all of the refrigerants

        // *************** GLYCOLS ***************
        // Go through each glycol found in the fluid names statement and read in the data
        // Note that every valid fluid must have ALL of the necessary data or a fatal error will
        // be produced.
        CurrentModuleObject = "FluidProperties:Concentration";
        for (Loop = 1; Loop <= state.dataFluidProps->NumOfGlycols; ++Loop) {

            // Get: ***** SPECIFIC HEAT of GLYCOLS  *****
            // First find the number of concentration value syntax lines have been entered and
            // make sure that all of the concentration input is linked to the same temperature list
            TempsName = "";
            FirstSHMatch = true;
            NumOfConcPts = 0;
            state.dataFluidProps->GlyRawData(Loop).CpDataPresent = false;
            for (InData = 1; InData <= NumOfGlyFluidPropArrays; ++InData) { // check temperatures given for specific heat are consistant
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->GlyRawData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), SpecificHeat))) {
                    ++NumOfConcPts;
                    if (FirstSHMatch) {
                        TempsName = Alphas(3);
                        FirstSHMatch = false;
                    } else {
                        if (!UtilityRoutines::SameString(TempsName, Alphas(3))) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                            ShowContinueError(state, "All glycol specific heat data for the same glycol must use the same temperature list");
                            ShowContinueError(state, "Expected name=" + TempsName + ", Entered name=" + Alphas(3));
                            ErrorsFound = true;
                        }
                    }
                }
            }
            if (NumOfConcPts > 0) {
                // Now allocate the arrays and read the data into the proper place
                // First, allocate the temperature array and transfer the data from the FluidTemp array
                state.dataFluidProps->GlyRawData(Loop).CpDataPresent = true;
                for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {
                    if (UtilityRoutines::SameString(TempsName, FluidTemps(TempLoop).Name)) {
                        state.dataFluidProps->GlyRawData(Loop).NumCpTempPts = FluidTemps(TempLoop).NumOfTemps;
                        state.dataFluidProps->GlyRawData(Loop).CpTemps.allocate(state.dataFluidProps->GlyRawData(Loop).NumCpTempPts);
                        state.dataFluidProps->GlyRawData(Loop).CpTemps = FluidTemps(TempLoop).Temps;
                        break; // the TempLoop DO loop
                    }
                    if (TempLoop == NumOfFluidTempArrays) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                        ShowContinueError(state, "No match for temperature array name found with glycol data");
                        ErrorsFound = true;
                    }
                }

                // Next, allocate the specific heat related arrays
                state.dataFluidProps->GlyRawData(Loop).NumCpConcPts = NumOfConcPts;
                state.dataFluidProps->GlyRawData(Loop).CpConcs.allocate(state.dataFluidProps->GlyRawData(Loop).NumCpConcPts);
                state.dataFluidProps->GlyRawData(Loop).CpValues.allocate(state.dataFluidProps->GlyRawData(Loop).NumCpConcPts,
                                                                         state.dataFluidProps->GlyRawData(Loop).NumCpTempPts);

                // Finally, get the specific heat and concentration values from the user input
                CurrentModuleObject = "FluidProperties:Concentration";
                NumOfConcPts = 0;
                for (InData = 1; InData <= NumOfGlyFluidPropArrays; ++InData) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             CurrentModuleObject,
                                                                             InData,
                                                                             Alphas,
                                                                             NumAlphas,
                                                                             Numbers,
                                                                             NumNumbers,
                                                                             Status,
                                                                             lNumericFieldBlanks,
                                                                             lAlphaFieldBlanks,
                                                                             cAlphaFieldNames,
                                                                             cNumericFieldNames);
                    if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->GlyRawData(Loop).Name)) &&
                        (UtilityRoutines::SameString(Alphas(2), SpecificHeat))) {
                        ++NumOfConcPts;
                        state.dataFluidProps->GlyRawData(Loop).CpConcs(NumOfConcPts) = Numbers(1);
                        // a little error trapping
                        if (NumOfConcPts == 1) {
                            if (state.dataFluidProps->GlyRawData(Loop).CpConcs(NumOfConcPts) < 0.0) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                                ShowContinueError(state, "Negative concentrations not allowed in fluid property input data");
                                ErrorsFound = true;
                            }
                        } else {
                            if (state.dataFluidProps->GlyRawData(Loop).CpConcs(NumOfConcPts) <=
                                state.dataFluidProps->GlyRawData(Loop).CpConcs(NumOfConcPts - 1)) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                                ShowContinueError(state, "Concentrations must be entered in ascending order for fluid property data");
                                ErrorsFound = true;
                            }
                        }
                        if ((NumNumbers - 1) == state.dataFluidProps->GlyRawData(Loop).NumCpTempPts) {
                            state.dataFluidProps->GlyRawData(Loop).CpValues(NumOfConcPts, {1, state.dataFluidProps->GlyRawData(Loop).NumCpTempPts}) =
                                Numbers({2, NumNumbers});
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                            ShowContinueError(state, "Number of specific heat data points not equal to number of temperature points");
                            ErrorsFound = true;
                        }
                    }
                }
            }
            // Get: ***** DENSITY of GLYCOLS  *****
            // First find the number of concentration value syntax lines have been entered and
            // make sure that all of the concentration input is linked to the same temperature list
            TempsName = "";
            FirstSHMatch = true;
            NumOfConcPts = 0;
            state.dataFluidProps->GlyRawData(Loop).RhoDataPresent = false;
            CurrentModuleObject = "FluidProperties:Concentration";
            for (InData = 1; InData <= NumOfGlyFluidPropArrays; ++InData) { // check temperatures given for density are consistant
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->GlyRawData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Density))) {
                    ++NumOfConcPts;
                    if (FirstSHMatch) {
                        TempsName = Alphas(3);
                        FirstSHMatch = false;
                    } else {
                        if (!UtilityRoutines::SameString(TempsName, Alphas(3))) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                            ShowContinueError(state, "All glycol density data for the same glycol must use the same temperature list");
                            ShowContinueError(state, "Expected name=" + TempsName + ", Entered name=" + Alphas(3));
                            ErrorsFound = true;
                        }
                    }
                }
            }
            if (NumOfConcPts > 0) {
                // Now allocate the arrays and read the data into the proper place
                // First, allocate the temperature array and transfer the data from the FluidTemp array
                state.dataFluidProps->GlyRawData(Loop).RhoDataPresent = true;
                for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {
                    if (UtilityRoutines::SameString(TempsName, FluidTemps(TempLoop).Name)) {
                        state.dataFluidProps->GlyRawData(Loop).NumRhoTempPts = FluidTemps(TempLoop).NumOfTemps;
                        state.dataFluidProps->GlyRawData(Loop).RhoTemps.allocate(state.dataFluidProps->GlyRawData(Loop).NumRhoTempPts);
                        state.dataFluidProps->GlyRawData(Loop).RhoTemps = FluidTemps(TempLoop).Temps;
                        break; // the TempLoop DO loop
                    }
                    if (TempLoop == NumOfFluidTempArrays) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                        ShowContinueError(state, "No match for temperature array name found with glycol data");
                        ErrorsFound = true;
                    }
                }

                // Next, allocate the density related arrays
                state.dataFluidProps->GlyRawData(Loop).NumRhoConcPts = NumOfConcPts;
                state.dataFluidProps->GlyRawData(Loop).RhoConcs.allocate(state.dataFluidProps->GlyRawData(Loop).NumRhoConcPts);
                state.dataFluidProps->GlyRawData(Loop).RhoValues.allocate(state.dataFluidProps->GlyRawData(Loop).NumRhoConcPts,
                                                                          state.dataFluidProps->GlyRawData(Loop).NumRhoTempPts);

                // Finally, get the density and concentration values from the user input
                NumOfConcPts = 0;
                CurrentModuleObject = "FluidProperties:Concentration";
                for (InData = 1; InData <= NumOfGlyFluidPropArrays; ++InData) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             CurrentModuleObject,
                                                                             InData,
                                                                             Alphas,
                                                                             NumAlphas,
                                                                             Numbers,
                                                                             NumNumbers,
                                                                             Status,
                                                                             lNumericFieldBlanks,
                                                                             lAlphaFieldBlanks,
                                                                             cAlphaFieldNames,
                                                                             cNumericFieldNames);
                    if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->GlyRawData(Loop).Name)) &&
                        (UtilityRoutines::SameString(Alphas(2), Density))) {
                        ++NumOfConcPts;
                        state.dataFluidProps->GlyRawData(Loop).RhoConcs(NumOfConcPts) = Numbers(1);
                        // a little error trapping
                        if (NumOfConcPts == 1) {
                            if (state.dataFluidProps->GlyRawData(Loop).RhoConcs(NumOfConcPts) < 0.0) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                                ShowContinueError(state, "Negative concentrations not allowed in fluid property input data");
                                ErrorsFound = true;
                            }
                        } else {
                            if (state.dataFluidProps->GlyRawData(Loop).RhoConcs(NumOfConcPts) <=
                                state.dataFluidProps->GlyRawData(Loop).RhoConcs(NumOfConcPts - 1)) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                                ShowContinueError(state, "Concentrations must be entered in ascending order for fluid property data");
                                ErrorsFound = true;
                            }
                        }
                        if ((NumNumbers - 1) == state.dataFluidProps->GlyRawData(Loop).NumRhoTempPts) {
                            state.dataFluidProps->GlyRawData(Loop).RhoValues(
                                NumOfConcPts, {1, state.dataFluidProps->GlyRawData(Loop).NumRhoTempPts}) = Numbers({2, NumNumbers});
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                            ShowContinueError(state, "Number of density data points not equal to number of temperature points");
                            ErrorsFound = true;
                        }
                    }
                }
            }
            // Get: ***** CONDUCTIVITY of GLYCOLS  *****
            // First find the number of concentration value syntax lines have been entered and
            // make sure that all of the concentration input is linked to the same temperature list
            TempsName = "";
            FirstSHMatch = true;
            NumOfConcPts = 0;
            state.dataFluidProps->GlyRawData(Loop).CondDataPresent = false;
            CurrentModuleObject = "FluidProperties:Concentration";
            for (InData = 1; InData <= NumOfGlyFluidPropArrays; ++InData) { // check temperatures given for conductivity are consistant
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->GlyRawData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Conductivity))) {
                    ++NumOfConcPts;
                    if (FirstSHMatch) {
                        TempsName = Alphas(3);
                        FirstSHMatch = false;
                    } else {
                        if (!UtilityRoutines::SameString(TempsName, Alphas(3))) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                            ShowContinueError(state, "All glycol conductivity data for the same glycol must use the same temperature list");
                            ShowContinueError(state, "Expected name=" + TempsName + ", Entered name=" + Alphas(3));
                            ErrorsFound = true;
                        }
                    }
                }
            }
            if (NumOfConcPts > 0) {
                // Now allocate the arrays and read the data into the proper place
                // First, allocate the temperature array and transfer the data from the FluidTemp array
                state.dataFluidProps->GlyRawData(Loop).CondDataPresent = true;
                for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {
                    if (UtilityRoutines::SameString(TempsName, FluidTemps(TempLoop).Name)) {
                        state.dataFluidProps->GlyRawData(Loop).NumCondTempPts = FluidTemps(TempLoop).NumOfTemps;
                        state.dataFluidProps->GlyRawData(Loop).CondTemps.allocate(state.dataFluidProps->GlyRawData(Loop).NumCondTempPts);
                        state.dataFluidProps->GlyRawData(Loop).CondTemps = FluidTemps(TempLoop).Temps;
                        break; // the TempLoop DO loop
                    }
                    if (TempLoop == NumOfFluidTempArrays) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                        ShowContinueError(state, "No match for temperature array name found with glycol data");
                        ErrorsFound = true;
                    }
                }

                // Next, allocate the conductivity related arrays
                state.dataFluidProps->GlyRawData(Loop).NumCondConcPts = NumOfConcPts;
                state.dataFluidProps->GlyRawData(Loop).CondConcs.allocate(state.dataFluidProps->GlyRawData(Loop).NumCondConcPts);
                state.dataFluidProps->GlyRawData(Loop).CondValues.allocate(state.dataFluidProps->GlyRawData(Loop).NumCondConcPts,
                                                                           state.dataFluidProps->GlyRawData(Loop).NumCondTempPts);

                // Finally, get the conductivity and concentration values from the user input
                NumOfConcPts = 0;
                CurrentModuleObject = "FluidProperties:Concentration";
                for (InData = 1; InData <= NumOfGlyFluidPropArrays; ++InData) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             CurrentModuleObject,
                                                                             InData,
                                                                             Alphas,
                                                                             NumAlphas,
                                                                             Numbers,
                                                                             NumNumbers,
                                                                             Status,
                                                                             lNumericFieldBlanks,
                                                                             lAlphaFieldBlanks,
                                                                             cAlphaFieldNames,
                                                                             cNumericFieldNames);
                    if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->GlyRawData(Loop).Name)) &&
                        (UtilityRoutines::SameString(Alphas(2), Conductivity))) {
                        ++NumOfConcPts;
                        state.dataFluidProps->GlyRawData(Loop).CondConcs(NumOfConcPts) = Numbers(1);
                        // a little error trapping
                        if (NumOfConcPts == 1) {
                            if (state.dataFluidProps->GlyRawData(Loop).CondConcs(NumOfConcPts) < 0.0) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                                ShowContinueError(state, "Negative concentrations not allowed in fluid property input data");
                                ErrorsFound = true;
                            }
                        } else {
                            if (state.dataFluidProps->GlyRawData(Loop).CondConcs(NumOfConcPts) <=
                                state.dataFluidProps->GlyRawData(Loop).CondConcs(NumOfConcPts - 1)) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                                ShowContinueError(state, "Concentrations must be entered in ascending order for fluid property data");
                                ErrorsFound = true;
                            }
                        }
                        if ((NumNumbers - 1) == state.dataFluidProps->GlyRawData(Loop).NumCondTempPts) {
                            state.dataFluidProps->GlyRawData(Loop).CondValues(
                                NumOfConcPts, {1, state.dataFluidProps->GlyRawData(Loop).NumCondTempPts}) = Numbers({2, NumNumbers});
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                            ShowContinueError(state, "Number of conductivity data points not equal to number of temperature points");
                            ErrorsFound = true;
                        }
                    }
                }
            }
            // Get: ***** VISCOSITY of GLYCOLS  *****
            // First find the number of concentration value syntax lines have been entered and
            // make sure that all of the concentration input is linked to the same temperature list
            TempsName = "";
            FirstSHMatch = true;
            NumOfConcPts = 0;
            state.dataFluidProps->GlyRawData(Loop).ViscDataPresent = false;
            CurrentModuleObject = "FluidProperties:Concentration";
            for (InData = 1; InData <= NumOfGlyFluidPropArrays; ++InData) { // check temperatures given for viscosity are consistant
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         InData,
                                                                         Alphas,
                                                                         NumAlphas,
                                                                         Numbers,
                                                                         NumNumbers,
                                                                         Status,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->GlyRawData(Loop).Name)) &&
                    (UtilityRoutines::SameString(Alphas(2), Viscosity))) {
                    ++NumOfConcPts;
                    if (FirstSHMatch) {
                        TempsName = Alphas(3);
                        FirstSHMatch = false;
                    } else {
                        if (!UtilityRoutines::SameString(TempsName, Alphas(3))) {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                            ShowContinueError(state, "All glycol viscosity data for the same glycol must use the same temperature list");
                            ShowContinueError(state, "Expected name=" + TempsName + ", Entered name=" + Alphas(3));
                            ErrorsFound = true;
                        }
                    }
                }
            }
            if (NumOfConcPts > 0) {
                state.dataFluidProps->GlyRawData(Loop).ViscDataPresent = true;
                // Now allocate the arrays and read the data into the proper place
                // First, allocate the temperature array and transfer the data from the FluidTemp array
                for (TempLoop = 1; TempLoop <= NumOfFluidTempArrays; ++TempLoop) {
                    if (UtilityRoutines::SameString(TempsName, FluidTemps(TempLoop).Name)) {
                        state.dataFluidProps->GlyRawData(Loop).NumViscTempPts = FluidTemps(TempLoop).NumOfTemps;
                        state.dataFluidProps->GlyRawData(Loop).ViscTemps.allocate(state.dataFluidProps->GlyRawData(Loop).NumViscTempPts);
                        state.dataFluidProps->GlyRawData(Loop).ViscTemps = FluidTemps(TempLoop).Temps;
                        break; // the TempLoop DO loop
                    }
                    if (TempLoop == NumOfFluidTempArrays) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                        ShowContinueError(state, "No match for temperature array name found with glycol data");
                        ErrorsFound = true;
                    }
                }

                // Next, allocate the viscosity related arrays
                state.dataFluidProps->GlyRawData(Loop).NumViscConcPts = NumOfConcPts;
                state.dataFluidProps->GlyRawData(Loop).ViscConcs.allocate(state.dataFluidProps->GlyRawData(Loop).NumViscConcPts);
                state.dataFluidProps->GlyRawData(Loop).ViscValues.allocate(state.dataFluidProps->GlyRawData(Loop).NumViscConcPts,
                                                                           state.dataFluidProps->GlyRawData(Loop).NumViscTempPts);

                // Finally, get the viscosity and concentration values from the user input
                NumOfConcPts = 0;
                CurrentModuleObject = "FluidProperties:Concentration";
                for (InData = 1; InData <= NumOfGlyFluidPropArrays; ++InData) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             CurrentModuleObject,
                                                                             InData,
                                                                             Alphas,
                                                                             NumAlphas,
                                                                             Numbers,
                                                                             NumNumbers,
                                                                             Status,
                                                                             lNumericFieldBlanks,
                                                                             lAlphaFieldBlanks,
                                                                             cAlphaFieldNames,
                                                                             cNumericFieldNames);
                    if ((UtilityRoutines::SameString(Alphas(1), state.dataFluidProps->GlyRawData(Loop).Name)) &&
                        (UtilityRoutines::SameString(Alphas(2), Viscosity))) {
                        ++NumOfConcPts;
                        state.dataFluidProps->GlyRawData(Loop).ViscConcs(NumOfConcPts) = Numbers(1);
                        // a little error trapping
                        if (NumOfConcPts == 1) {
                            if (state.dataFluidProps->GlyRawData(Loop).ViscConcs(NumOfConcPts) < 0.0) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                                ShowContinueError(state, "Negative concentrations not allowed in fluid property input data");
                                ErrorsFound = true;
                            }
                        } else {
                            if (state.dataFluidProps->GlyRawData(Loop).ViscConcs(NumOfConcPts) <=
                                state.dataFluidProps->GlyRawData(Loop).ViscConcs(NumOfConcPts - 1)) {
                                ShowSevereError(
                                    state, std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                                ShowContinueError(state, "Concentrations must be entered in ascending order for fluid property data");
                                ErrorsFound = true;
                            }
                        }
                        if ((NumNumbers - 1) == state.dataFluidProps->GlyRawData(Loop).NumViscTempPts) {
                            state.dataFluidProps->GlyRawData(Loop).ViscValues(
                                NumOfConcPts, {1, state.dataFluidProps->GlyRawData(Loop).NumViscTempPts}) = Numbers({2, NumNumbers});
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + " Name=" + state.dataFluidProps->GlyRawData(Loop).Name);
                            ShowContinueError(state, "Number of viscosity data points not equal to number of temperature points");
                            ErrorsFound = true;
                        }
                    }
                }
            }
        } // glycol loop

        // Get: ***** GLYCOL CONCENTRATIONS *****
        // Read in the GlycolConcentrations input and then set the property data accordingly
        // Input Syntax:
        // FluidProperties:GlycolConcentration,
        //       \memo glycol and what concentration it is
        //  A1,  \field Name
        //       \type alpha
        //       \required-field
        //       \reference GlycolConcentrations
        //  A2,  \field Glycol Type
        //       \required-field
        //       \type choice
        //       \key EthyleneGlycol
        //       \key PropyleneGlycol
        //       \key UserDefinedGlycolType
        //       \note or UserDefined Fluid (must show up as a glycol in FluidProperties:Name object)
        //  A3,  \field User Defined Glycol Name
        //       \type object-list
        //       \object-list FluidAndGlycolNames
        //  N1;  \field Glycol Concentration
        //       \type real
        //       \minimum 0.0
        //       \maximum 1.0

        // Check to see if there is any GlycolConcentrations input.  If not, this
        // is okay as long as the user only desires to simulate loops with water.
        // More than one GlycolConcentrations input is not allowed.

        CurrentModuleObject = "FluidProperties:GlycolConcentration";
        NumOfOptionalInput = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        NumOfGlyConcs = NumOfOptionalInput + 1;
        state.dataFluidProps->GlycolData.allocate(NumOfGlyConcs);
        state.dataFluidProps->GlycolUsed.dimension(NumOfGlyConcs, false);

        state.dataFluidProps->GlycolUsed(1) = true; // mark Water as always used

        // First "glycol" is always pure water.  Load data from default arrays
        state.dataFluidProps->GlycolData(1).Name = "WATER";
        state.dataFluidProps->GlycolData(1).GlycolName = "WATER";
        state.dataFluidProps->GlycolData(1).GlycolIndex = 0;
        state.dataFluidProps->GlycolData(1).Concentration = 1.0;
        state.dataFluidProps->GlycolData(1).CpDataPresent = true;
        state.dataFluidProps->GlycolData(1).NumCpTempPts = DefaultNumGlyTemps;
        state.dataFluidProps->GlycolData(1).RhoDataPresent = true;
        state.dataFluidProps->GlycolData(1).NumRhoTempPts = DefaultNumGlyTemps;
        state.dataFluidProps->GlycolData(1).CondDataPresent = true;
        state.dataFluidProps->GlycolData(1).NumCondTempPts = DefaultNumGlyTemps;
        state.dataFluidProps->GlycolData(1).ViscDataPresent = true;
        state.dataFluidProps->GlycolData(1).NumViscTempPts = DefaultNumGlyTemps;
        state.dataFluidProps->GlycolData(1).CpTemps.allocate(state.dataFluidProps->GlycolData(1).NumCpTempPts);
        state.dataFluidProps->GlycolData(1).CpValues.allocate(state.dataFluidProps->GlycolData(1).NumCpTempPts);
        state.dataFluidProps->GlycolData(1).RhoTemps.allocate(state.dataFluidProps->GlycolData(1).NumRhoTempPts);
        state.dataFluidProps->GlycolData(1).RhoValues.allocate(state.dataFluidProps->GlycolData(1).NumRhoTempPts);
        state.dataFluidProps->GlycolData(1).CondTemps.allocate(state.dataFluidProps->GlycolData(1).NumCondTempPts);
        state.dataFluidProps->GlycolData(1).CondValues.allocate(state.dataFluidProps->GlycolData(1).NumCondTempPts);
        state.dataFluidProps->GlycolData(1).ViscTemps.allocate(state.dataFluidProps->GlycolData(1).NumViscTempPts);
        state.dataFluidProps->GlycolData(1).ViscValues.allocate(state.dataFluidProps->GlycolData(1).NumViscTempPts);
        state.dataFluidProps->GlycolData(1).CpTemps = DefaultGlycolTemps;
        state.dataFluidProps->GlycolData(1).CpValues = DefaultWaterCpData;
        state.dataFluidProps->GlycolData(1).RhoTemps = DefaultGlycolTemps;
        state.dataFluidProps->GlycolData(1).RhoValues = DefaultWaterRhoData;
        state.dataFluidProps->GlycolData(1).CondTemps = DefaultGlycolTemps;
        state.dataFluidProps->GlycolData(1).CondValues = DefaultWaterCondData;
        state.dataFluidProps->GlycolData(1).ViscTemps = DefaultGlycolTemps;
        state.dataFluidProps->GlycolData(1).ViscValues = DefaultWaterViscData;

        NumOfGlyConcs = 1; // Water is always available, everything else must be specified

        for (Loop = 1; Loop <= NumOfOptionalInput; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     Loop,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     Status,
                                                                     lNumericFieldBlanks,
                                                                     lAlphaFieldBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);
            if (UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound)) {
                continue;
            }
            GlycolFound = false;
            if (UtilityRoutines::SameString(Alphas(2), EthyleneGlycol)) {
                GlycolFound = true;
                ++NumOfGlyConcs;
                state.dataFluidProps->GlycolData(NumOfGlyConcs).Name = Alphas(1);
                state.dataFluidProps->GlycolData(NumOfGlyConcs).GlycolName = Alphas(2);
            } else if (UtilityRoutines::SameString(Alphas(2), PropyleneGlycol)) {
                GlycolFound = true;
                ++NumOfGlyConcs;
                state.dataFluidProps->GlycolData(NumOfGlyConcs).Name = Alphas(1);
                state.dataFluidProps->GlycolData(NumOfGlyConcs).GlycolName = Alphas(2);
            } else if (UtilityRoutines::SameString(Alphas(2), "UserDefinedGlycolType")) {
                for (InData = 1; InData <= state.dataFluidProps->NumOfGlycols; ++InData) {
                    if (UtilityRoutines::SameString(Alphas(3), state.dataFluidProps->GlyRawData(InData).Name)) {
                        GlycolFound = true;
                        break; // DO LOOP through user defined glycols
                    }
                }
                if (GlycolFound) {
                    ++NumOfGlyConcs;
                    state.dataFluidProps->GlycolData(NumOfGlyConcs).Name = Alphas(1);
                    state.dataFluidProps->GlycolData(NumOfGlyConcs).GlycolName = Alphas(3);
                } else {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid reference");
                    ShowContinueError(state, "... not found in the FluidProperties:Name list: \"" + Alphas(3) + "\".");
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid field");
                ShowContinueError(state, "..." + cAlphaFieldNames(2) + "=\"" + Alphas(2) + "\".");
                ShowContinueError(state, "... Legal values are PropyleneGlycol, EthyleneGlycol or UserDefinedGlycolType.");
                ErrorsFound = true;
            }
            if (!GlycolFound) continue;
            state.dataFluidProps->GlycolData(NumOfGlyConcs).Concentration = Numbers(1);
        }

        // Now initialize the rest of the data for the glycols
        for (Loop = 2; Loop <= NumOfGlyConcs; ++Loop) {
            // Check to see if glycol name is one of the defaults or is listed in the Fluid Name list
            if (UtilityRoutines::SameString(state.dataFluidProps->GlycolData(Loop).GlycolName, EthyleneGlycol)) {
                state.dataFluidProps->GlycolData(Loop).GlycolIndex = EthyleneGlycolIndex;
            } else if (UtilityRoutines::SameString(state.dataFluidProps->GlycolData(Loop).GlycolName, PropyleneGlycol)) {
                state.dataFluidProps->GlycolData(Loop).GlycolIndex = PropyleneGlycolIndex;
            } else {
                for (InData = 1; InData <= state.dataFluidProps->NumOfGlycols; ++InData) {
                    if (UtilityRoutines::SameString(state.dataFluidProps->GlycolData(Loop).GlycolName,
                                                    state.dataFluidProps->GlyRawData(InData).Name)) {
                        state.dataFluidProps->GlycolData(Loop).GlycolIndex = InData;
                        break; // DO LOOP through user defined glycols
                    }
                }
            }

            // Set the rest of the parameters...
            if ((state.dataFluidProps->GlycolData(Loop).GlycolIndex == EthyleneGlycolIndex) ||
                (state.dataFluidProps->GlycolData(Loop).GlycolIndex == PropyleneGlycolIndex)) {

                state.dataFluidProps->GlycolData(Loop).CpDataPresent = true;
                state.dataFluidProps->GlycolData(Loop).NumCpTempPts = DefaultNumGlyTemps;
                state.dataFluidProps->GlycolData(Loop).RhoDataPresent = true;
                state.dataFluidProps->GlycolData(Loop).NumRhoTempPts = DefaultNumGlyTemps;
                state.dataFluidProps->GlycolData(Loop).CondDataPresent = true;
                state.dataFluidProps->GlycolData(Loop).NumCondTempPts = DefaultNumGlyTemps;
                state.dataFluidProps->GlycolData(Loop).ViscDataPresent = true;
                state.dataFluidProps->GlycolData(Loop).NumViscTempPts = DefaultNumGlyTemps;
                state.dataFluidProps->GlycolData(Loop).CpTemps.allocate(state.dataFluidProps->GlycolData(Loop).NumCpTempPts);
                state.dataFluidProps->GlycolData(Loop).CpValues.allocate(state.dataFluidProps->GlycolData(Loop).NumCpTempPts);
                state.dataFluidProps->GlycolData(Loop).RhoTemps.allocate(state.dataFluidProps->GlycolData(Loop).NumRhoTempPts);
                state.dataFluidProps->GlycolData(Loop).RhoValues.allocate(state.dataFluidProps->GlycolData(Loop).NumRhoTempPts);
                state.dataFluidProps->GlycolData(Loop).CondTemps.allocate(state.dataFluidProps->GlycolData(Loop).NumCondTempPts);
                state.dataFluidProps->GlycolData(Loop).CondValues.allocate(state.dataFluidProps->GlycolData(Loop).NumCondTempPts);
                state.dataFluidProps->GlycolData(Loop).ViscTemps.allocate(state.dataFluidProps->GlycolData(Loop).NumViscTempPts);
                state.dataFluidProps->GlycolData(Loop).ViscValues.allocate(state.dataFluidProps->GlycolData(Loop).NumViscTempPts);
                state.dataFluidProps->GlycolData(Loop).CpTemps = DefaultGlycolTemps;
                state.dataFluidProps->GlycolData(Loop).RhoTemps = DefaultGlycolTemps;
                state.dataFluidProps->GlycolData(Loop).CondTemps = DefaultGlycolTemps;
                state.dataFluidProps->GlycolData(Loop).ViscTemps = DefaultGlycolTemps;

                if (state.dataFluidProps->GlycolData(Loop).GlycolIndex == EthyleneGlycolIndex) {
                    InterpDefValuesForGlycolConc(state,
                                                 DefaultGlycolConcs,
                                                 DefaultEthGlyCpData,
                                                 state.dataFluidProps->GlycolData(Loop).Concentration,
                                                 state.dataFluidProps->GlycolData(Loop).CpValues);
                    InterpDefValuesForGlycolConc(state,
                                                 DefaultGlycolConcs,
                                                 DefaultEthGlyRhoData,
                                                 state.dataFluidProps->GlycolData(Loop).Concentration,
                                                 state.dataFluidProps->GlycolData(Loop).RhoValues);
                    InterpDefValuesForGlycolConc(state,
                                                 DefaultGlycolConcs,
                                                 DefaultEthGlyCondData,
                                                 state.dataFluidProps->GlycolData(Loop).Concentration,
                                                 state.dataFluidProps->GlycolData(Loop).CondValues);
                    InterpDefValuesForGlycolConc(state,
                                                 DefaultGlycolConcs,
                                                 DefaultEthGlyViscData,
                                                 state.dataFluidProps->GlycolData(Loop).Concentration,
                                                 state.dataFluidProps->GlycolData(Loop).ViscValues);
                } else { // == PropyleneGlycolIndex
                    InterpDefValuesForGlycolConc(state,
                                                 DefaultGlycolConcs,
                                                 DefaultPropGlyCpData,
                                                 state.dataFluidProps->GlycolData(Loop).Concentration,
                                                 state.dataFluidProps->GlycolData(Loop).CpValues);
                    InterpDefValuesForGlycolConc(state,
                                                 DefaultGlycolConcs,
                                                 DefaultPropGlyRhoData,
                                                 state.dataFluidProps->GlycolData(Loop).Concentration,
                                                 state.dataFluidProps->GlycolData(Loop).RhoValues);
                    InterpDefValuesForGlycolConc(state,
                                                 DefaultGlycolConcs,
                                                 DefaultPropGlyCondData,
                                                 state.dataFluidProps->GlycolData(Loop).Concentration,
                                                 state.dataFluidProps->GlycolData(Loop).CondValues);
                    InterpDefValuesForGlycolConc(state,
                                                 DefaultGlycolConcs,
                                                 DefaultPropGlyViscData,
                                                 state.dataFluidProps->GlycolData(Loop).Concentration,
                                                 state.dataFluidProps->GlycolData(Loop).ViscValues);
                }

            } else { // User-defined fluid

                Index = state.dataFluidProps->GlycolData(Loop).GlycolIndex;

                // Specific heat data:
                if (state.dataFluidProps->GlyRawData(Index).CpDataPresent) {
                    state.dataFluidProps->GlycolData(Loop).CpDataPresent = true;
                    state.dataFluidProps->GlycolData(Loop).NumCpTempPts = state.dataFluidProps->GlyRawData(Index).NumCpTempPts;
                    state.dataFluidProps->GlycolData(Loop).CpTemps.allocate(state.dataFluidProps->GlycolData(Loop).NumCpTempPts);
                    state.dataFluidProps->GlycolData(Loop).CpValues.allocate(state.dataFluidProps->GlycolData(Loop).NumCpTempPts);
                    state.dataFluidProps->GlycolData(Loop).CpTemps = state.dataFluidProps->GlyRawData(Index).CpTemps;
                    InterpValuesForGlycolConc(state,
                                              state.dataFluidProps->GlyRawData(Index).NumCpConcPts,
                                              state.dataFluidProps->GlyRawData(Index).NumCpTempPts,
                                              state.dataFluidProps->GlyRawData(Index).CpConcs,
                                              state.dataFluidProps->GlyRawData(Index).CpValues,
                                              state.dataFluidProps->GlycolData(Loop).Concentration,
                                              state.dataFluidProps->GlycolData(Loop).CpValues);
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Specific heat data not entered for a " + CurrentModuleObject);
                    ShowContinueError(state, "ALL data must be entered for user-defined glycols");
                    ShowContinueError(state, "Glycol mixture name = " + state.dataFluidProps->GlycolData(Loop).Name);
                    ShowContinueError(state, "Glycol fluid name = " + state.dataFluidProps->GlycolData(Loop).GlycolName);
                    ErrorsFound = true;
                }

                // Density data:
                if (state.dataFluidProps->GlyRawData(Index).CpDataPresent) {
                    state.dataFluidProps->GlycolData(Loop).RhoDataPresent = true;
                    state.dataFluidProps->GlycolData(Loop).NumRhoTempPts = state.dataFluidProps->GlyRawData(Index).NumRhoTempPts;
                    state.dataFluidProps->GlycolData(Loop).RhoTemps.allocate(state.dataFluidProps->GlycolData(Loop).NumRhoTempPts);
                    state.dataFluidProps->GlycolData(Loop).RhoValues.allocate(state.dataFluidProps->GlycolData(Loop).NumRhoTempPts);
                    state.dataFluidProps->GlycolData(Loop).RhoTemps = state.dataFluidProps->GlyRawData(Index).RhoTemps;
                    InterpValuesForGlycolConc(state,
                                              state.dataFluidProps->GlyRawData(Index).NumRhoConcPts,
                                              state.dataFluidProps->GlyRawData(Index).NumRhoTempPts,
                                              state.dataFluidProps->GlyRawData(Index).RhoConcs,
                                              state.dataFluidProps->GlyRawData(Index).RhoValues,
                                              state.dataFluidProps->GlycolData(Loop).Concentration,
                                              state.dataFluidProps->GlycolData(Loop).RhoValues);
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Density data not entered for a " + CurrentModuleObject);
                    ShowContinueError(state, "ALL data must be entered for user-defined glycols");
                    ShowContinueError(state, "Glycol mixture name = " + state.dataFluidProps->GlycolData(Loop).Name);
                    ShowContinueError(state, "Glycol fluid name = " + state.dataFluidProps->GlycolData(Loop).GlycolName);
                    ErrorsFound = true;
                }

                // Conductivity data:
                if (state.dataFluidProps->GlyRawData(Index).CondDataPresent) {
                    state.dataFluidProps->GlycolData(Loop).CondDataPresent = true;
                    state.dataFluidProps->GlycolData(Loop).NumCondTempPts = state.dataFluidProps->GlyRawData(Index).NumCondTempPts;
                    state.dataFluidProps->GlycolData(Loop).CondTemps.allocate(state.dataFluidProps->GlycolData(Loop).NumCondTempPts);
                    state.dataFluidProps->GlycolData(Loop).CondValues.allocate(state.dataFluidProps->GlycolData(Loop).NumCondTempPts);
                    state.dataFluidProps->GlycolData(Loop).CondTemps = state.dataFluidProps->GlyRawData(Index).CondTemps;
                    InterpValuesForGlycolConc(state,
                                              state.dataFluidProps->GlyRawData(Index).NumCondConcPts,
                                              state.dataFluidProps->GlyRawData(Index).NumCondTempPts,
                                              state.dataFluidProps->GlyRawData(Index).CondConcs,
                                              state.dataFluidProps->GlyRawData(Index).CondValues,
                                              state.dataFluidProps->GlycolData(Loop).Concentration,
                                              state.dataFluidProps->GlycolData(Loop).CondValues);
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Conductivity data not entered for a " + CurrentModuleObject);
                    ShowContinueError(state, "ALL data must be entered for user-defined glycols");
                    ShowContinueError(state, "Glycol mixture name = " + state.dataFluidProps->GlycolData(Loop).Name);
                    ShowContinueError(state, "Glycol fluid name = " + state.dataFluidProps->GlycolData(Loop).GlycolName);
                    ErrorsFound = true;
                }

                // Viscosity data:
                if (state.dataFluidProps->GlyRawData(Index).ViscDataPresent) {
                    state.dataFluidProps->GlycolData(Loop).ViscDataPresent = true;
                    state.dataFluidProps->GlycolData(Loop).NumViscTempPts = state.dataFluidProps->GlyRawData(Index).NumViscTempPts;
                    state.dataFluidProps->GlycolData(Loop).ViscTemps.allocate(state.dataFluidProps->GlycolData(Loop).NumViscTempPts);
                    state.dataFluidProps->GlycolData(Loop).ViscValues.allocate(state.dataFluidProps->GlycolData(Loop).NumViscTempPts);
                    state.dataFluidProps->GlycolData(Loop).ViscTemps = state.dataFluidProps->GlyRawData(Index).ViscTemps;
                    InterpValuesForGlycolConc(state,
                                              state.dataFluidProps->GlyRawData(Index).NumViscConcPts,
                                              state.dataFluidProps->GlyRawData(Index).NumViscTempPts,
                                              state.dataFluidProps->GlyRawData(Index).ViscConcs,
                                              state.dataFluidProps->GlyRawData(Index).ViscValues,
                                              state.dataFluidProps->GlycolData(Loop).Concentration,
                                              state.dataFluidProps->GlycolData(Loop).ViscValues);
                } else {
                    ShowSevereError(state, std::string{RoutineName} + "Viscosity data not entered for a " + CurrentModuleObject);
                    ShowContinueError(state, "ALL data must be entered for user-defined glycols");
                    ShowContinueError(state, "Glycol mixture name = " + state.dataFluidProps->GlycolData(Loop).Name);
                    ShowContinueError(state, "Glycol fluid name = " + state.dataFluidProps->GlycolData(Loop).GlycolName);
                    ErrorsFound = true;
                }
            }
        }

        state.dataFluidProps->NumOfGlycols = NumOfGlyConcs; // Reset number of glycols to actual number
        state.dataFluidProps->GlycolErrorTracking.allocate(state.dataFluidProps->NumOfGlycols);
        for (std::size_t i = 0; i < state.dataFluidProps->GlycolErrorTracking.size(); ++i)
            state.dataFluidProps->GlycolErrorTracking[i].Name = state.dataFluidProps->GlycolData[i].Name;

        if (!ErrorsFound) InitializeGlycolTempLimits(state, ErrorsFound); // Initialize the Temp limits for the glycols

        if (!ErrorsFound) InitializeRefrigerantLimits(state, ErrorsFound); // Initialize the limits for the refrigerants

        FluidTemps.deallocate();

        Alphas.deallocate();
        cAlphaFieldNames.deallocate();
        lAlphaFieldBlanks.deallocate();
        Numbers.deallocate();
        cNumericFieldNames.deallocate();
        lNumericFieldBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Previous errors in input cause program termination.");
        }

        if (state.dataInputProcessing->inputProcessor->getNumSectionsFound("REPORTGLYCOLS") > 0) state.dataFluidProps->DebugReportGlycols = true;
        if (state.dataInputProcessing->inputProcessor->getNumSectionsFound("REPORTREFRIGERANTS") > 0)
            state.dataFluidProps->DebugReportRefrigerants = true;
        if (state.dataInputProcessing->inputProcessor->getNumSectionsFound("INCREASEGLYCOLERRORLIMIT") > 0)
            state.dataFluidProps->GlycolErrorLimitTest += 10;
        if (state.dataInputProcessing->inputProcessor->getNumSectionsFound("INCREASEREFRIGERANTERRORLIMIT") > 0)
            state.dataFluidProps->RefrigerantErrorLimitTest += 10;

        if (state.dataFluidProps->DebugReportGlycols) ReportAndTestGlycols(state);
        if (state.dataFluidProps->DebugReportRefrigerants) ReportAndTestRefrigerants(state);
    }

    [[maybe_unused]] constexpr std::array<std::array<Real64, DefaultNumSteamSuperheatedTemps>, DefaultNumSteamSuperheatedPressure>
        DefaultSteamSuperheatedEnthalpyDataTable = {{
            {2501000.0, 2503000.0, 2510000.0, 2520000.0, 2529000.0, 2538000.0, 2548000.0, 2557000.0, 2566000.0, 2576000.0, 2585000.0, 2595000.0,
             2604000.0, 2613000.0, 2623000.0, 2632000.0, 2636000.0, 2640000.0, 2643000.0, 2647000.0, 2651000.0, 2655000.0, 2658000.0, 2662000.0,
             2666000.0, 2670000.0, 2673000.0, 2677000.0, 2681000.0, 2685000.0, 2687000.0, 2689000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0,
             2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0,
             2721000.0, 2723000.0, 2725000.0, 2727000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2740000.0, 2742000.0,
             2744000.0, 2746000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0, 2784000.0,
             2788000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2830000.0,
             2834000.0, 2837000.0, 2841000.0, 2851000.0, 2861000.0, 2870000.0, 2880000.0, 2890000.0, 2899000.0, 2909000.0, 2919000.0, 2929000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       2503000.0, 2510000.0, 2520000.0, 2529000.0, 2538000.0, 2548000.0, 2557000.0, 2566000.0, 2576000.0, 2585000.0, 2595000.0,
             2604000.0, 2613000.0, 2623000.0, 2632000.0, 2636000.0, 2640000.0, 2643000.0, 2647000.0, 2651000.0, 2655000.0, 2658000.0, 2662000.0,
             2666000.0, 2670000.0, 2673000.0, 2677000.0, 2681000.0, 2685000.0, 2687000.0, 2689000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0,
             2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0,
             2721000.0, 2723000.0, 2725000.0, 2727000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2740000.0, 2742000.0,
             2744000.0, 2746000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0, 2784000.0,
             2788000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2830000.0,
             2834000.0, 2837000.0, 2841000.0, 2851000.0, 2861000.0, 2870000.0, 2880000.0, 2890000.0, 2899000.0, 2909000.0, 2919000.0, 2929000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       2510000.0, 2519000.0, 2529000.0, 2538000.0, 2548000.0, 2557000.0, 2566000.0, 2576000.0, 2585000.0, 2594000.0,
             2604000.0, 2613000.0, 2623000.0, 2632000.0, 2636000.0, 2640000.0, 2643000.0, 2647000.0, 2651000.0, 2655000.0, 2658000.0, 2662000.0,
             2666000.0, 2670000.0, 2673000.0, 2677000.0, 2681000.0, 2685000.0, 2687000.0, 2689000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0,
             2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0,
             2721000.0, 2723000.0, 2725000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2740000.0, 2742000.0,
             2744000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0, 2784000.0,
             2788000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2830000.0,
             2834000.0, 2837000.0, 2841000.0, 2851000.0, 2861000.0, 2870000.0, 2880000.0, 2890000.0, 2899000.0, 2909000.0, 2919000.0, 2929000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       2519000.0, 2529000.0, 2538000.0, 2547000.0, 2557000.0, 2566000.0, 2576000.0, 2585000.0, 2594000.0,
             2604000.0, 2613000.0, 2623000.0, 2632000.0, 2636000.0, 2639000.0, 2643000.0, 2647000.0, 2651000.0, 2655000.0, 2658000.0, 2662000.0,
             2666000.0, 2670000.0, 2673000.0, 2677000.0, 2681000.0, 2685000.0, 2687000.0, 2689000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0,
             2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0,
             2721000.0, 2723000.0, 2725000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2740000.0, 2742000.0,
             2744000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0, 2784000.0,
             2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2830000.0,
             2834000.0, 2837000.0, 2841000.0, 2851000.0, 2861000.0, 2870000.0, 2880000.0, 2890000.0, 2899000.0, 2909000.0, 2919000.0, 2929000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       2528000.0, 2538000.0, 2547000.0, 2557000.0, 2566000.0, 2575000.0, 2585000.0, 2594000.0,
             2604000.0, 2613000.0, 2622000.0, 2632000.0, 2636000.0, 2639000.0, 2643000.0, 2647000.0, 2651000.0, 2654000.0, 2658000.0, 2662000.0,
             2666000.0, 2670000.0, 2673000.0, 2677000.0, 2681000.0, 2685000.0, 2687000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0,
             2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0,
             2721000.0, 2723000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2740000.0, 2742000.0,
             2744000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0, 2784000.0,
             2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2830000.0,
             2834000.0, 2837000.0, 2841000.0, 2851000.0, 2861000.0, 2870000.0, 2880000.0, 2890000.0, 2899000.0, 2909000.0, 2919000.0, 2929000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       2537000.0, 2547000.0, 2556000.0, 2566000.0, 2575000.0, 2585000.0, 2594000.0,
             2603000.0, 2613000.0, 2622000.0, 2632000.0, 2635000.0, 2639000.0, 2643000.0, 2647000.0, 2651000.0, 2654000.0, 2658000.0, 2662000.0,
             2666000.0, 2669000.0, 2673000.0, 2677000.0, 2681000.0, 2685000.0, 2687000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0,
             2698000.0, 2700000.0, 2702000.0, 2704000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0,
             2721000.0, 2723000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2740000.0, 2742000.0,
             2743000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0, 2784000.0,
             2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2830000.0,
             2834000.0, 2837000.0, 2841000.0, 2851000.0, 2861000.0, 2870000.0, 2880000.0, 2890000.0, 2899000.0, 2909000.0, 2919000.0, 2929000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2547000.0, 2556000.0, 2566000.0, 2575000.0, 2584000.0, 2594000.0,
             2603000.0, 2613000.0, 2622000.0, 2632000.0, 2635000.0, 2639000.0, 2643000.0, 2647000.0, 2650000.0, 2654000.0, 2658000.0, 2662000.0,
             2666000.0, 2669000.0, 2673000.0, 2677000.0, 2681000.0, 2685000.0, 2686000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0,
             2698000.0, 2700000.0, 2702000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0,
             2721000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2740000.0, 2741000.0,
             2743000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0, 2784000.0,
             2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2830000.0,
             2833000.0, 2837000.0, 2841000.0, 2851000.0, 2861000.0, 2870000.0, 2880000.0, 2890000.0, 2899000.0, 2909000.0, 2919000.0, 2929000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2556000.0, 2565000.0, 2575000.0, 2584000.0, 2594000.0,
             2603000.0, 2612000.0, 2622000.0, 2631000.0, 2635000.0, 2639000.0, 2643000.0, 2646000.0, 2650000.0, 2654000.0, 2658000.0, 2662000.0,
             2665000.0, 2669000.0, 2673000.0, 2677000.0, 2681000.0, 2684000.0, 2686000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0,
             2698000.0, 2700000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0,
             2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2739000.0, 2741000.0,
             2743000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0, 2783000.0,
             2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2830000.0,
             2833000.0, 2837000.0, 2841000.0, 2851000.0, 2860000.0, 2870000.0, 2880000.0, 2890000.0, 2899000.0, 2909000.0, 2919000.0, 2929000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2565000.0, 2574000.0, 2584000.0, 2593000.0,
             2603000.0, 2612000.0, 2622000.0, 2631000.0, 2635000.0, 2639000.0, 2642000.0, 2646000.0, 2650000.0, 2654000.0, 2658000.0, 2661000.0,
             2665000.0, 2669000.0, 2673000.0, 2677000.0, 2680000.0, 2684000.0, 2686000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0,
             2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2718000.0,
             2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2737000.0, 2739000.0, 2741000.0,
             2743000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0, 2783000.0,
             2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2829000.0,
             2833000.0, 2837000.0, 2841000.0, 2851000.0, 2860000.0, 2870000.0, 2880000.0, 2890000.0, 2899000.0, 2909000.0, 2919000.0, 2929000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2574000.0, 2583000.0, 2593000.0,
             2602000.0, 2612000.0, 2621000.0, 2631000.0, 2635000.0, 2638000.0, 2642000.0, 2646000.0, 2650000.0, 2654000.0, 2657000.0, 2661000.0,
             2665000.0, 2669000.0, 2673000.0, 2676000.0, 2680000.0, 2684000.0, 2686000.0, 2688000.0, 2690000.0, 2692000.0, 2693000.0, 2695000.0,
             2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2714000.0, 2716000.0, 2718000.0,
             2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2733000.0, 2735000.0, 2737000.0, 2739000.0, 2741000.0,
             2743000.0, 2745000.0, 2749000.0, 2753000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0, 2779000.0, 2783000.0,
             2787000.0, 2791000.0, 2795000.0, 2799000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0, 2829000.0,
             2833000.0, 2837000.0, 2841000.0, 2851000.0, 2860000.0, 2870000.0, 2880000.0, 2889000.0, 2899000.0, 2909000.0, 2919000.0, 2928000.0,
             2938000.0, 2958000.0, 2978000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2582000.0, 2592000.0,
             2602000.0, 2611000.0, 2621000.0, 2630000.0, 2634000.0, 2638000.0, 2642000.0, 2646000.0, 2649000.0, 2653000.0, 2657000.0, 2661000.0,
             2665000.0, 2668000.0, 2672000.0, 2676000.0, 2680000.0, 2684000.0, 2686000.0, 2688000.0, 2689000.0, 2691000.0, 2693000.0, 2695000.0,
             2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0,
             2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2729000.0, 2731000.0, 2733000.0, 2735000.0, 2737000.0, 2739000.0, 2741000.0,
             2743000.0, 2745000.0, 2749000.0, 2752000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0, 2775000.0, 2779000.0, 2783000.0,
             2787000.0, 2791000.0, 2795000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2825000.0, 2829000.0,
             2833000.0, 2837000.0, 2841000.0, 2851000.0, 2860000.0, 2870000.0, 2880000.0, 2889000.0, 2899000.0, 2909000.0, 2919000.0, 2928000.0,
             2938000.0, 2958000.0, 2977000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2591000.0,
             2601000.0, 2611000.0, 2620000.0, 2630000.0, 2634000.0, 2637000.0, 2641000.0, 2645000.0, 2649000.0, 2653000.0, 2657000.0, 2660000.0,
             2664000.0, 2668000.0, 2672000.0, 2676000.0, 2680000.0, 2683000.0, 2685000.0, 2687000.0, 2689000.0, 2691000.0, 2693000.0, 2695000.0,
             2697000.0, 2699000.0, 2701000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0,
             2720000.0, 2722000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0, 2735000.0, 2737000.0, 2739000.0, 2741000.0,
             2743000.0, 2745000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2771000.0, 2775000.0, 2779000.0, 2783000.0,
             2787000.0, 2791000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2821000.0, 2825000.0, 2829000.0,
             2833000.0, 2837000.0, 2841000.0, 2850000.0, 2860000.0, 2870000.0, 2879000.0, 2889000.0, 2899000.0, 2909000.0, 2918000.0, 2928000.0,
             2938000.0, 2958000.0, 2977000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3178000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             2600000.0, 2610000.0, 2620000.0, 2629000.0, 2633000.0, 2637000.0, 2641000.0, 2645000.0, 2648000.0, 2652000.0, 2656000.0, 2660000.0,
             2664000.0, 2668000.0, 2671000.0, 2675000.0, 2679000.0, 2683000.0, 2685000.0, 2687000.0, 2689000.0, 2691000.0, 2692000.0, 2694000.0,
             2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0, 2715000.0, 2717000.0,
             2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0, 2735000.0, 2737000.0, 2738000.0, 2740000.0,
             2742000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2763000.0, 2767000.0, 2771000.0, 2775000.0, 2779000.0, 2783000.0,
             2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2813000.0, 2817000.0, 2821000.0, 2825000.0, 2829000.0,
             2833000.0, 2837000.0, 2841000.0, 2850000.0, 2860000.0, 2870000.0, 2879000.0, 2889000.0, 2899000.0, 2909000.0, 2918000.0, 2928000.0,
             2938000.0, 2958000.0, 2977000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3077000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3177000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       2609000.0, 2619000.0, 2628000.0, 2632000.0, 2636000.0, 2640000.0, 2644000.0, 2648000.0, 2652000.0, 2655000.0, 2659000.0,
             2663000.0, 2667000.0, 2671000.0, 2675000.0, 2679000.0, 2682000.0, 2684000.0, 2686000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0,
             2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0,
             2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2740000.0,
             2742000.0, 2744000.0, 2748000.0, 2752000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0, 2771000.0, 2775000.0, 2778000.0, 2782000.0,
             2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2805000.0, 2809000.0, 2813000.0, 2817000.0, 2821000.0, 2825000.0, 2829000.0,
             2833000.0, 2836000.0, 2840000.0, 2850000.0, 2860000.0, 2869000.0, 2879000.0, 2889000.0, 2899000.0, 2908000.0, 2918000.0, 2928000.0,
             2938000.0, 2957000.0, 2977000.0, 2997000.0, 3017000.0, 3037000.0, 3057000.0, 3076000.0, 3097000.0, 3117000.0, 3137000.0, 3157000.0,
             3177000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       2618000.0, 2627000.0, 2631000.0, 2635000.0, 2639000.0, 2643000.0, 2647000.0, 2651000.0, 2655000.0, 2659000.0,
             2662000.0, 2666000.0, 2670000.0, 2674000.0, 2678000.0, 2682000.0, 2684000.0, 2686000.0, 2688000.0, 2689000.0, 2691000.0, 2693000.0,
             2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2716000.0,
             2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0, 2740000.0,
             2741000.0, 2743000.0, 2747000.0, 2751000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0, 2770000.0, 2774000.0, 2778000.0, 2782000.0,
             2786000.0, 2790000.0, 2794000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0, 2813000.0, 2817000.0, 2821000.0, 2825000.0, 2828000.0,
             2832000.0, 2836000.0, 2840000.0, 2850000.0, 2859000.0, 2869000.0, 2879000.0, 2889000.0, 2898000.0, 2908000.0, 2918000.0, 2928000.0,
             2938000.0, 2957000.0, 2977000.0, 2997000.0, 3017000.0, 3036000.0, 3056000.0, 3076000.0, 3096000.0, 3117000.0, 3137000.0, 3157000.0,
             3177000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3490000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       2626000.0, 2630000.0, 2634000.0, 2638000.0, 2642000.0, 2646000.0, 2650000.0, 2654000.0, 2658000.0,
             2661000.0, 2665000.0, 2669000.0, 2673000.0, 2677000.0, 2681000.0, 2683000.0, 2685000.0, 2687000.0, 2689000.0, 2691000.0, 2693000.0,
             2695000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0, 2716000.0,
             2718000.0, 2720000.0, 2722000.0, 2724000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0, 2735000.0, 2737000.0, 2739000.0,
             2741000.0, 2743000.0, 2747000.0, 2751000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2770000.0, 2774000.0, 2778000.0, 2782000.0,
             2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0, 2813000.0, 2816000.0, 2820000.0, 2824000.0, 2828000.0,
             2832000.0, 2836000.0, 2840000.0, 2849000.0, 2859000.0, 2869000.0, 2879000.0, 2888000.0, 2898000.0, 2908000.0, 2918000.0, 2928000.0,
             2937000.0, 2957000.0, 2977000.0, 2997000.0, 3016000.0, 3036000.0, 3056000.0, 3076000.0, 3096000.0, 3116000.0, 3137000.0, 3157000.0,
             3177000.0, 3198000.0, 3218000.0, 3280000.0, 3384000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       2630000.0, 2633000.0, 2637000.0, 2641000.0, 2645000.0, 2649000.0, 2653000.0, 2657000.0,
             2661000.0, 2665000.0, 2669000.0, 2673000.0, 2677000.0, 2681000.0, 2683000.0, 2684000.0, 2686000.0, 2688000.0, 2690000.0, 2692000.0,
             2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0, 2716000.0,
             2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0, 2735000.0, 2737000.0, 2739000.0,
             2741000.0, 2743000.0, 2747000.0, 2750000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2770000.0, 2774000.0, 2777000.0, 2781000.0,
             2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2805000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0, 2824000.0, 2828000.0,
             2832000.0, 2836000.0, 2840000.0, 2849000.0, 2859000.0, 2869000.0, 2879000.0, 2888000.0, 2898000.0, 2908000.0, 2918000.0, 2927000.0,
             2937000.0, 2957000.0, 2977000.0, 2996000.0, 3016000.0, 3036000.0, 3056000.0, 3076000.0, 3096000.0, 3116000.0, 3137000.0, 3157000.0,
             3177000.0, 3197000.0, 3218000.0, 3280000.0, 3384000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       2633000.0, 2637000.0, 2641000.0, 2645000.0, 2649000.0, 2653000.0, 2657000.0,
             2661000.0, 2665000.0, 2668000.0, 2672000.0, 2676000.0, 2680000.0, 2682000.0, 2684000.0, 2686000.0, 2688000.0, 2690000.0, 2692000.0,
             2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0,
             2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0, 2735000.0, 2737000.0, 2738000.0,
             2740000.0, 2742000.0, 2746000.0, 2750000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2770000.0, 2773000.0, 2777000.0, 2781000.0,
             2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0, 2824000.0, 2828000.0,
             2832000.0, 2835000.0, 2839000.0, 2849000.0, 2859000.0, 2869000.0, 2878000.0, 2888000.0, 2898000.0, 2908000.0, 2918000.0, 2927000.0,
             2937000.0, 2957000.0, 2977000.0, 2996000.0, 3016000.0, 3036000.0, 3056000.0, 3076000.0, 3096000.0, 3116000.0, 3136000.0, 3157000.0,
             3177000.0, 3197000.0, 3218000.0, 3280000.0, 3384000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2636000.0, 2640000.0, 2644000.0, 2648000.0, 2652000.0, 2656000.0,
             2660000.0, 2664000.0, 2668000.0, 2672000.0, 2676000.0, 2680000.0, 2682000.0, 2684000.0, 2686000.0, 2688000.0, 2690000.0, 2691000.0,
             2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0,
             2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0,
             2740000.0, 2742000.0, 2746000.0, 2750000.0, 2754000.0, 2758000.0, 2762000.0, 2765000.0, 2769000.0, 2773000.0, 2777000.0, 2781000.0,
             2785000.0, 2789000.0, 2793000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0, 2824000.0, 2828000.0,
             2831000.0, 2835000.0, 2839000.0, 2849000.0, 2859000.0, 2868000.0, 2878000.0, 2888000.0, 2898000.0, 2908000.0, 2917000.0, 2927000.0,
             2937000.0, 2957000.0, 2976000.0, 2996000.0, 3016000.0, 3036000.0, 3056000.0, 3076000.0, 3096000.0, 3116000.0, 3136000.0, 3157000.0,
             3177000.0, 3197000.0, 3218000.0, 3280000.0, 3384000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2640000.0, 2644000.0, 2648000.0, 2652000.0, 2656000.0,
             2660000.0, 2664000.0, 2667000.0, 2671000.0, 2675000.0, 2679000.0, 2681000.0, 2683000.0, 2685000.0, 2687000.0, 2689000.0, 2691000.0,
             2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0,
             2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0,
             2740000.0, 2742000.0, 2746000.0, 2750000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2769000.0, 2773000.0, 2777000.0, 2781000.0,
             2785000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0, 2823000.0, 2827000.0,
             2831000.0, 2835000.0, 2839000.0, 2849000.0, 2859000.0, 2868000.0, 2878000.0, 2888000.0, 2898000.0, 2907000.0, 2917000.0, 2927000.0,
             2937000.0, 2957000.0, 2976000.0, 2996000.0, 3016000.0, 3036000.0, 3056000.0, 3076000.0, 3096000.0, 3116000.0, 3136000.0, 3157000.0,
             3177000.0, 3197000.0, 3218000.0, 3280000.0, 3384000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2643000.0, 2647000.0, 2651000.0, 2655000.0,
             2659000.0, 2663000.0, 2667000.0, 2671000.0, 2675000.0, 2679000.0, 2681000.0, 2683000.0, 2685000.0, 2687000.0, 2689000.0, 2691000.0,
             2693000.0, 2695000.0, 2697000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0,
             2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0, 2738000.0,
             2740000.0, 2741000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2769000.0, 2773000.0, 2777000.0, 2780000.0,
             2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2815000.0, 2819000.0, 2823000.0, 2827000.0,
             2831000.0, 2835000.0, 2839000.0, 2849000.0, 2858000.0, 2868000.0, 2878000.0, 2888000.0, 2897000.0, 2907000.0, 2917000.0, 2927000.0,
             2937000.0, 2956000.0, 2976000.0, 2996000.0, 3016000.0, 3036000.0, 3056000.0, 3076000.0, 3096000.0, 3116000.0, 3136000.0, 3157000.0,
             3177000.0, 3197000.0, 3218000.0, 3280000.0, 3384000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2646000.0, 2650000.0, 2654000.0,
             2658000.0, 2662000.0, 2666000.0, 2670000.0, 2674000.0, 2678000.0, 2680000.0, 2682000.0, 2684000.0, 2686000.0, 2688000.0, 2690000.0,
             2692000.0, 2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0,
             2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0, 2735000.0, 2737000.0,
             2739000.0, 2741000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0,
             2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2807000.0, 2811000.0, 2815000.0, 2819000.0, 2823000.0, 2827000.0,
             2831000.0, 2835000.0, 2839000.0, 2848000.0, 2858000.0, 2868000.0, 2878000.0, 2887000.0, 2897000.0, 2907000.0, 2917000.0, 2927000.0,
             2937000.0, 2956000.0, 2976000.0, 2996000.0, 3016000.0, 3036000.0, 3056000.0, 3076000.0, 3096000.0, 3116000.0, 3136000.0, 3156000.0,
             3177000.0, 3197000.0, 3218000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2650000.0, 2654000.0,
             2658000.0, 2662000.0, 2666000.0, 2670000.0, 2674000.0, 2678000.0, 2680000.0, 2682000.0, 2684000.0, 2686000.0, 2688000.0, 2690000.0,
             2692000.0, 2694000.0, 2696000.0, 2698000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0,
             2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0, 2735000.0, 2737000.0,
             2739000.0, 2741000.0, 2745000.0, 2749000.0, 2752000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0, 2780000.0,
             2784000.0, 2788000.0, 2792000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2815000.0, 2819000.0, 2823000.0, 2827000.0,
             2831000.0, 2834000.0, 2838000.0, 2848000.0, 2858000.0, 2868000.0, 2878000.0, 2887000.0, 2897000.0, 2907000.0, 2917000.0, 2927000.0,
             2936000.0, 2956000.0, 2976000.0, 2996000.0, 3016000.0, 3036000.0, 3056000.0, 3076000.0, 3096000.0, 3116000.0, 3136000.0, 3156000.0,
             3177000.0, 3197000.0, 3218000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2653000.0,
             2657000.0, 2661000.0, 2665000.0, 2669000.0, 2673000.0, 2677000.0, 2679000.0, 2681000.0, 2683000.0, 2685000.0, 2687000.0, 2689000.0,
             2691000.0, 2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0,
             2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2732000.0, 2734000.0, 2736000.0,
             2738000.0, 2740000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0, 2779000.0,
             2783000.0, 2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2815000.0, 2819000.0, 2822000.0, 2826000.0,
             2830000.0, 2834000.0, 2838000.0, 2848000.0, 2858000.0, 2867000.0, 2877000.0, 2887000.0, 2897000.0, 2907000.0, 2917000.0, 2926000.0,
             2936000.0, 2956000.0, 2976000.0, 2996000.0, 3016000.0, 3035000.0, 3055000.0, 3076000.0, 3096000.0, 3116000.0, 3136000.0, 3156000.0,
             3177000.0, 3197000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             2656000.0, 2660000.0, 2664000.0, 2668000.0, 2672000.0, 2676000.0, 2678000.0, 2680000.0, 2682000.0, 2684000.0, 2686000.0, 2688000.0,
             2690000.0, 2692000.0, 2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0,
             2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2736000.0,
             2738000.0, 2740000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2763000.0, 2767000.0, 2771000.0, 2775000.0, 2779000.0,
             2783000.0, 2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0,
             2830000.0, 2834000.0, 2838000.0, 2848000.0, 2857000.0, 2867000.0, 2877000.0, 2887000.0, 2897000.0, 2907000.0, 2916000.0, 2926000.0,
             2936000.0, 2956000.0, 2976000.0, 2996000.0, 3015000.0, 3035000.0, 3055000.0, 3075000.0, 3095000.0, 3116000.0, 3136000.0, 3156000.0,
             3176000.0, 3197000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       2660000.0, 2664000.0, 2668000.0, 2672000.0, 2676000.0, 2678000.0, 2680000.0, 2682000.0, 2684000.0, 2686000.0, 2688000.0,
             2690000.0, 2692000.0, 2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0,
             2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0, 2735000.0,
             2737000.0, 2739000.0, 2743000.0, 2747000.0, 2751000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0, 2771000.0, 2775000.0, 2779000.0,
             2783000.0, 2787000.0, 2791000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0,
             2830000.0, 2834000.0, 2838000.0, 2847000.0, 2857000.0, 2867000.0, 2877000.0, 2887000.0, 2896000.0, 2906000.0, 2916000.0, 2926000.0,
             2936000.0, 2956000.0, 2975000.0, 2995000.0, 3015000.0, 3035000.0, 3055000.0, 3075000.0, 3095000.0, 3116000.0, 3136000.0, 3156000.0,
             3176000.0, 3197000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       2663000.0, 2667000.0, 2671000.0, 2675000.0, 2677000.0, 2679000.0, 2681000.0, 2683000.0, 2685000.0, 2687000.0,
             2689000.0, 2691000.0, 2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0,
             2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0, 2735000.0,
             2737000.0, 2739000.0, 2743000.0, 2747000.0, 2751000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0, 2771000.0, 2774000.0, 2778000.0,
             2782000.0, 2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0, 2826000.0,
             2829000.0, 2833000.0, 2837000.0, 2847000.0, 2857000.0, 2867000.0, 2877000.0, 2886000.0, 2896000.0, 2906000.0, 2916000.0, 2926000.0,
             2936000.0, 2955000.0, 2975000.0, 2995000.0, 3015000.0, 3035000.0, 3055000.0, 3075000.0, 3095000.0, 3115000.0, 3136000.0, 3156000.0,
             3176000.0, 3197000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       2666000.0, 2670000.0, 2674000.0, 2676000.0, 2678000.0, 2680000.0, 2682000.0, 2684000.0, 2686000.0,
             2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0,
             2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0,
             2736000.0, 2738000.0, 2742000.0, 2746000.0, 2750000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2770000.0, 2774000.0, 2778000.0,
             2782000.0, 2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2809000.0, 2813000.0, 2817000.0, 2821000.0, 2825000.0,
             2829000.0, 2833000.0, 2837000.0, 2847000.0, 2857000.0, 2866000.0, 2876000.0, 2886000.0, 2896000.0, 2906000.0, 2916000.0, 2926000.0,
             2935000.0, 2955000.0, 2975000.0, 2995000.0, 3015000.0, 3035000.0, 3055000.0, 3075000.0, 3095000.0, 3115000.0, 3136000.0, 3156000.0,
             3176000.0, 3197000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       2669000.0, 2673000.0, 2675000.0, 2677000.0, 2679000.0, 2682000.0, 2684000.0, 2686000.0,
             2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0,
             2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0, 2734000.0,
             2736000.0, 2738000.0, 2742000.0, 2746000.0, 2750000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2770000.0, 2774000.0, 2777000.0,
             2781000.0, 2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0, 2813000.0, 2817000.0, 2821000.0, 2825000.0,
             2829000.0, 2833000.0, 2837000.0, 2846000.0, 2856000.0, 2866000.0, 2876000.0, 2886000.0, 2896000.0, 2906000.0, 2915000.0, 2925000.0,
             2935000.0, 2955000.0, 2975000.0, 2995000.0, 3015000.0, 3035000.0, 3055000.0, 3075000.0, 3095000.0, 3115000.0, 3135000.0, 3156000.0,
             3176000.0, 3196000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       2672000.0, 2674000.0, 2677000.0, 2679000.0, 2681000.0, 2683000.0, 2685000.0,
             2687000.0, 2689000.0, 2691000.0, 2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0,
             2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0,
             2735000.0, 2737000.0, 2741000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2769000.0, 2773000.0, 2777000.0,
             2781000.0, 2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0, 2813000.0, 2817000.0, 2820000.0, 2824000.0,
             2828000.0, 2832000.0, 2836000.0, 2846000.0, 2856000.0, 2866000.0, 2876000.0, 2886000.0, 2895000.0, 2905000.0, 2915000.0, 2925000.0,
             2935000.0, 2955000.0, 2975000.0, 2995000.0, 3015000.0, 3035000.0, 3055000.0, 3075000.0, 3095000.0, 3115000.0, 3135000.0, 3156000.0,
             3176000.0, 3196000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2674000.0, 2676000.0, 2678000.0, 2680000.0, 2682000.0, 2684000.0,
             2686000.0, 2688000.0, 2690000.0, 2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0, 2709000.0,
             2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0,
             2735000.0, 2737000.0, 2741000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2769000.0, 2773000.0, 2777000.0,
             2781000.0, 2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0, 2824000.0,
             2828000.0, 2832000.0, 2836000.0, 2846000.0, 2856000.0, 2866000.0, 2876000.0, 2885000.0, 2895000.0, 2905000.0, 2915000.0, 2925000.0,
             2935000.0, 2955000.0, 2975000.0, 2994000.0, 3014000.0, 3034000.0, 3054000.0, 3075000.0, 3095000.0, 3115000.0, 3135000.0, 3156000.0,
             3176000.0, 3196000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2676000.0, 2678000.0, 2680000.0, 2682000.0, 2684000.0,
             2686000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0,
             2710000.0, 2712000.0, 2714000.0, 2716000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0, 2733000.0,
             2735000.0, 2737000.0, 2741000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2769000.0, 2773000.0, 2776000.0,
             2780000.0, 2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0, 2824000.0,
             2828000.0, 2832000.0, 2836000.0, 2846000.0, 2856000.0, 2866000.0, 2875000.0, 2885000.0, 2895000.0, 2905000.0, 2915000.0, 2925000.0,
             2935000.0, 2955000.0, 2974000.0, 2994000.0, 3014000.0, 3034000.0, 3054000.0, 3074000.0, 3095000.0, 3115000.0, 3135000.0, 3155000.0,
             3176000.0, 3196000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2677000.0, 2679000.0, 2681000.0, 2683000.0,
             2685000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0, 2708000.0,
             2710000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0,
             2734000.0, 2736000.0, 2740000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0,
             2780000.0, 2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0, 2824000.0,
             2828000.0, 2832000.0, 2836000.0, 2846000.0, 2855000.0, 2865000.0, 2875000.0, 2885000.0, 2895000.0, 2905000.0, 2915000.0, 2925000.0,
             2935000.0, 2954000.0, 2974000.0, 2994000.0, 3014000.0, 3034000.0, 3054000.0, 3074000.0, 3095000.0, 3115000.0, 3135000.0, 3155000.0,
             3176000.0, 3196000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2679000.0, 2681000.0, 2683000.0,
             2685000.0, 2687000.0, 2689000.0, 2691000.0, 2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2706000.0, 2708000.0,
             2710000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0, 2732000.0,
             2734000.0, 2736000.0, 2740000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0,
             2780000.0, 2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0, 2824000.0,
             2828000.0, 2832000.0, 2835000.0, 2845000.0, 2855000.0, 2865000.0, 2875000.0, 2885000.0, 2895000.0, 2905000.0, 2915000.0, 2925000.0,
             2934000.0, 2954000.0, 2974000.0, 2994000.0, 3014000.0, 3034000.0, 3054000.0, 3074000.0, 3094000.0, 3115000.0, 3135000.0, 3155000.0,
             3176000.0, 3196000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2680000.0, 2682000.0,
             2684000.0, 2687000.0, 2689000.0, 2691000.0, 2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0,
             2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2730000.0, 2732000.0,
             2734000.0, 2736000.0, 2740000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0, 2776000.0,
             2780000.0, 2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2807000.0, 2811000.0, 2815000.0, 2819000.0, 2823000.0,
             2827000.0, 2831000.0, 2835000.0, 2845000.0, 2855000.0, 2865000.0, 2875000.0, 2885000.0, 2895000.0, 2905000.0, 2914000.0, 2924000.0,
             2934000.0, 2954000.0, 2974000.0, 2994000.0, 3014000.0, 3034000.0, 3054000.0, 3074000.0, 3094000.0, 3115000.0, 3135000.0, 3155000.0,
             3176000.0, 3196000.0, 3217000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2682000.0,
             2684000.0, 2686000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0, 2698000.0, 2701000.0, 2703000.0, 2705000.0, 2707000.0,
             2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0,
             2733000.0, 2735000.0, 2739000.0, 2743000.0, 2747000.0, 2751000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0, 2771000.0, 2775000.0,
             2779000.0, 2783000.0, 2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2815000.0, 2819000.0, 2823000.0,
             2827000.0, 2831000.0, 2835000.0, 2845000.0, 2855000.0, 2865000.0, 2875000.0, 2885000.0, 2894000.0, 2904000.0, 2914000.0, 2924000.0,
             2934000.0, 2954000.0, 2974000.0, 2994000.0, 3014000.0, 3034000.0, 3054000.0, 3074000.0, 3094000.0, 3115000.0, 3135000.0, 3155000.0,
             3176000.0, 3196000.0, 3216000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             2683000.0, 2685000.0, 2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0,
             2708000.0, 2710000.0, 2712000.0, 2714000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0, 2731000.0,
             2733000.0, 2735000.0, 2739000.0, 2743000.0, 2747000.0, 2751000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0, 2771000.0, 2775000.0,
             2779000.0, 2783000.0, 2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2815000.0, 2819000.0, 2823000.0,
             2827000.0, 2831000.0, 2835000.0, 2845000.0, 2855000.0, 2865000.0, 2874000.0, 2884000.0, 2894000.0, 2904000.0, 2914000.0, 2924000.0,
             2934000.0, 2954000.0, 2974000.0, 2994000.0, 3014000.0, 3034000.0, 3054000.0, 3074000.0, 3094000.0, 3114000.0, 3135000.0, 3155000.0,
             3175000.0, 3196000.0, 3216000.0, 3280000.0, 3383000.0, 3489000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       2685000.0, 2687000.0, 2689000.0, 2691000.0, 2693000.0, 2695000.0, 2697000.0, 2700000.0, 2702000.0, 2704000.0, 2706000.0,
             2708000.0, 2710000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0,
             2732000.0, 2734000.0, 2738000.0, 2743000.0, 2747000.0, 2751000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0, 2771000.0, 2775000.0,
             2779000.0, 2783000.0, 2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2815000.0, 2819000.0, 2823000.0,
             2827000.0, 2831000.0, 2835000.0, 2844000.0, 2854000.0, 2864000.0, 2874000.0, 2884000.0, 2894000.0, 2904000.0, 2914000.0, 2924000.0,
             2934000.0, 2954000.0, 2974000.0, 2994000.0, 3014000.0, 3034000.0, 3054000.0, 3074000.0, 3094000.0, 3114000.0, 3135000.0, 3155000.0,
             3175000.0, 3196000.0, 3216000.0, 3280000.0, 3383000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       2686000.0, 2689000.0, 2691000.0, 2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0,
             2707000.0, 2709000.0, 2711000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0, 2730000.0,
             2732000.0, 2734000.0, 2738000.0, 2742000.0, 2746000.0, 2750000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2770000.0, 2774000.0,
             2778000.0, 2782000.0, 2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0,
             2826000.0, 2830000.0, 2834000.0, 2844000.0, 2854000.0, 2864000.0, 2874000.0, 2884000.0, 2894000.0, 2904000.0, 2914000.0, 2924000.0,
             2934000.0, 2954000.0, 2974000.0, 2994000.0, 3014000.0, 3034000.0, 3054000.0, 3074000.0, 3094000.0, 3114000.0, 3135000.0, 3155000.0,
             3175000.0, 3196000.0, 3216000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       2688000.0, 2690000.0, 2692000.0, 2694000.0, 2696000.0, 2699000.0, 2701000.0, 2703000.0, 2705000.0,
             2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2730000.0,
             2732000.0, 2734000.0, 2738000.0, 2742000.0, 2746000.0, 2750000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2770000.0, 2774000.0,
             2778000.0, 2782000.0, 2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0,
             2826000.0, 2830000.0, 2834000.0, 2844000.0, 2854000.0, 2864000.0, 2874000.0, 2884000.0, 2894000.0, 2904000.0, 2914000.0, 2924000.0,
             2934000.0, 2953000.0, 2973000.0, 2993000.0, 3013000.0, 3033000.0, 3054000.0, 3074000.0, 3094000.0, 3114000.0, 3134000.0, 3155000.0,
             3175000.0, 3196000.0, 3216000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       2690000.0, 2692000.0, 2694000.0, 2696000.0, 2698000.0, 2700000.0, 2702000.0, 2704000.0,
             2706000.0, 2708000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0, 2729000.0,
             2731000.0, 2733000.0, 2737000.0, 2741000.0, 2745000.0, 2749000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2770000.0, 2774000.0,
             2778000.0, 2782000.0, 2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0,
             2826000.0, 2830000.0, 2834000.0, 2844000.0, 2854000.0, 2864000.0, 2874000.0, 2884000.0, 2894000.0, 2903000.0, 2913000.0, 2923000.0,
             2933000.0, 2953000.0, 2973000.0, 2993000.0, 3013000.0, 3033000.0, 3053000.0, 3074000.0, 3094000.0, 3114000.0, 3134000.0, 3155000.0,
             3175000.0, 3196000.0, 3216000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       2691000.0, 2693000.0, 2695000.0, 2697000.0, 2700000.0, 2702000.0, 2704000.0,
             2706000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2727000.0, 2729000.0,
             2731000.0, 2733000.0, 2737000.0, 2741000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2769000.0, 2773000.0,
             2777000.0, 2781000.0, 2785000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0, 2822000.0,
             2826000.0, 2830000.0, 2834000.0, 2844000.0, 2853000.0, 2863000.0, 2873000.0, 2883000.0, 2893000.0, 2903000.0, 2913000.0, 2923000.0,
             2933000.0, 2953000.0, 2973000.0, 2993000.0, 3013000.0, 3033000.0, 3053000.0, 3073000.0, 3094000.0, 3114000.0, 3134000.0, 3155000.0,
             3175000.0, 3195000.0, 3216000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2693000.0, 2695000.0, 2697000.0, 2699000.0, 2701000.0, 2703000.0,
             2705000.0, 2707000.0, 2709000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0, 2728000.0,
             2730000.0, 2732000.0, 2736000.0, 2740000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2769000.0, 2773000.0,
             2777000.0, 2781000.0, 2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0, 2813000.0, 2817000.0, 2821000.0,
             2825000.0, 2829000.0, 2833000.0, 2843000.0, 2853000.0, 2863000.0, 2873000.0, 2883000.0, 2893000.0, 2903000.0, 2913000.0, 2923000.0,
             2933000.0, 2953000.0, 2973000.0, 2993000.0, 3013000.0, 3033000.0, 3053000.0, 3073000.0, 3094000.0, 3114000.0, 3134000.0, 3154000.0,
             3175000.0, 3195000.0, 3216000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2694000.0, 2696000.0, 2698000.0, 2700000.0, 2703000.0,
             2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2724000.0, 2726000.0, 2728000.0,
             2730000.0, 2732000.0, 2736000.0, 2740000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2765000.0, 2769000.0, 2773000.0,
             2777000.0, 2781000.0, 2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0, 2813000.0, 2817000.0, 2821000.0,
             2825000.0, 2829000.0, 2833000.0, 2843000.0, 2853000.0, 2863000.0, 2873000.0, 2883000.0, 2893000.0, 2903000.0, 2913000.0, 2923000.0,
             2933000.0, 2953000.0, 2973000.0, 2993000.0, 3013000.0, 3033000.0, 3053000.0, 3073000.0, 3093000.0, 3114000.0, 3134000.0, 3154000.0,
             3175000.0, 3195000.0, 3216000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2696000.0, 2698000.0, 2700000.0, 2702000.0,
             2704000.0, 2706000.0, 2708000.0, 2710000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0, 2727000.0,
             2729000.0, 2731000.0, 2735000.0, 2740000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0,
             2776000.0, 2780000.0, 2784000.0, 2788000.0, 2793000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0, 2813000.0, 2817000.0, 2821000.0,
             2825000.0, 2829000.0, 2833000.0, 2843000.0, 2853000.0, 2863000.0, 2873000.0, 2883000.0, 2893000.0, 2903000.0, 2913000.0, 2923000.0,
             2933000.0, 2953000.0, 2973000.0, 2993000.0, 3013000.0, 3033000.0, 3053000.0, 3073000.0, 3093000.0, 3114000.0, 3134000.0, 3154000.0,
             3175000.0, 3195000.0, 3216000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2697000.0, 2699000.0, 2701000.0,
             2703000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2725000.0, 2727000.0,
             2729000.0, 2731000.0, 2735000.0, 2739000.0, 2743000.0, 2747000.0, 2751000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0, 2772000.0,
             2776000.0, 2780000.0, 2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0,
             2824000.0, 2828000.0, 2832000.0, 2842000.0, 2852000.0, 2862000.0, 2872000.0, 2882000.0, 2892000.0, 2902000.0, 2912000.0, 2922000.0,
             2932000.0, 2952000.0, 2972000.0, 2992000.0, 3013000.0, 3033000.0, 3053000.0, 3073000.0, 3093000.0, 3113000.0, 3134000.0, 3154000.0,
             3175000.0, 3195000.0, 3216000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2699000.0, 2701000.0,
             2703000.0, 2705000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0, 2726000.0,
             2728000.0, 2730000.0, 2734000.0, 2739000.0, 2743000.0, 2747000.0, 2751000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0, 2771000.0,
             2776000.0, 2780000.0, 2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0,
             2824000.0, 2828000.0, 2832000.0, 2842000.0, 2852000.0, 2862000.0, 2872000.0, 2882000.0, 2892000.0, 2902000.0, 2912000.0, 2922000.0,
             2932000.0, 2952000.0, 2972000.0, 2992000.0, 3012000.0, 3032000.0, 3053000.0, 3073000.0, 3093000.0, 3113000.0, 3134000.0, 3154000.0,
             3174000.0, 3195000.0, 3216000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2700000.0,
             2702000.0, 2704000.0, 2706000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2726000.0,
             2728000.0, 2730000.0, 2734000.0, 2738000.0, 2742000.0, 2746000.0, 2750000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0, 2771000.0,
             2775000.0, 2779000.0, 2783000.0, 2787000.0, 2791000.0, 2795000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0, 2820000.0,
             2824000.0, 2828000.0, 2832000.0, 2842000.0, 2852000.0, 2862000.0, 2872000.0, 2882000.0, 2892000.0, 2902000.0, 2912000.0, 2922000.0,
             2932000.0, 2952000.0, 2972000.0, 2992000.0, 3012000.0, 3032000.0, 3052000.0, 3073000.0, 3093000.0, 3113000.0, 3134000.0, 3154000.0,
             3174000.0, 3195000.0, 3215000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             2702000.0, 2704000.0, 2706000.0, 2708000.0, 2710000.0, 2712000.0, 2714000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0, 2725000.0,
             2727000.0, 2729000.0, 2733000.0, 2738000.0, 2742000.0, 2746000.0, 2750000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2771000.0,
             2775000.0, 2779000.0, 2783000.0, 2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2815000.0, 2819000.0,
             2823000.0, 2827000.0, 2831000.0, 2842000.0, 2852000.0, 2862000.0, 2872000.0, 2882000.0, 2892000.0, 2902000.0, 2912000.0, 2922000.0,
             2932000.0, 2952000.0, 2972000.0, 2992000.0, 3012000.0, 3032000.0, 3052000.0, 3073000.0, 3093000.0, 3113000.0, 3133000.0, 3154000.0,
             3174000.0, 3195000.0, 3215000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       2703000.0, 2705000.0, 2707000.0, 2709000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0, 2724000.0,
             2726000.0, 2729000.0, 2733000.0, 2737000.0, 2741000.0, 2745000.0, 2749000.0, 2754000.0, 2758000.0, 2762000.0, 2766000.0, 2770000.0,
             2774000.0, 2778000.0, 2782000.0, 2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2815000.0, 2819000.0,
             2823000.0, 2827000.0, 2831000.0, 2841000.0, 2851000.0, 2861000.0, 2871000.0, 2881000.0, 2891000.0, 2901000.0, 2911000.0, 2922000.0,
             2932000.0, 2952000.0, 2972000.0, 2992000.0, 3012000.0, 3032000.0, 3052000.0, 3072000.0, 3093000.0, 3113000.0, 3133000.0, 3154000.0,
             3174000.0, 3195000.0, 3215000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       2704000.0, 2707000.0, 2709000.0, 2711000.0, 2713000.0, 2715000.0, 2717000.0, 2720000.0, 2722000.0, 2724000.0,
             2726000.0, 2728000.0, 2732000.0, 2736000.0, 2741000.0, 2745000.0, 2749000.0, 2753000.0, 2757000.0, 2761000.0, 2766000.0, 2770000.0,
             2774000.0, 2778000.0, 2782000.0, 2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2811000.0, 2815000.0, 2819000.0,
             2823000.0, 2827000.0, 2831000.0, 2841000.0, 2851000.0, 2861000.0, 2871000.0, 2881000.0, 2891000.0, 2901000.0, 2911000.0, 2921000.0,
             2931000.0, 2951000.0, 2971000.0, 2992000.0, 3012000.0, 3032000.0, 3052000.0, 3072000.0, 3093000.0, 3113000.0, 3133000.0, 3154000.0,
             3174000.0, 3195000.0, 3215000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       2706000.0, 2708000.0, 2710000.0, 2712000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0, 2723000.0,
             2725000.0, 2727000.0, 2732000.0, 2736000.0, 2740000.0, 2744000.0, 2748000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0, 2769000.0,
             2773000.0, 2777000.0, 2782000.0, 2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0,
             2822000.0, 2826000.0, 2830000.0, 2841000.0, 2851000.0, 2861000.0, 2871000.0, 2881000.0, 2891000.0, 2901000.0, 2911000.0, 2921000.0,
             2931000.0, 2951000.0, 2971000.0, 2991000.0, 3012000.0, 3032000.0, 3052000.0, 3072000.0, 3092000.0, 3113000.0, 3133000.0, 3153000.0,
             3174000.0, 3194000.0, 3215000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       2707000.0, 2710000.0, 2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0, 2722000.0,
             2725000.0, 2727000.0, 2731000.0, 2735000.0, 2739000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2760000.0, 2765000.0, 2769000.0,
             2773000.0, 2777000.0, 2781000.0, 2785000.0, 2789000.0, 2793000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0, 2818000.0,
             2822000.0, 2826000.0, 2830000.0, 2840000.0, 2850000.0, 2860000.0, 2870000.0, 2881000.0, 2891000.0, 2901000.0, 2911000.0, 2921000.0,
             2931000.0, 2951000.0, 2971000.0, 2991000.0, 3011000.0, 3031000.0, 3052000.0, 3072000.0, 3092000.0, 3113000.0, 3133000.0, 3153000.0,
             3174000.0, 3194000.0, 3215000.0, 3280000.0, 3382000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       2709000.0, 2711000.0, 2713000.0, 2715000.0, 2718000.0, 2720000.0, 2722000.0,
             2724000.0, 2726000.0, 2730000.0, 2735000.0, 2739000.0, 2743000.0, 2747000.0, 2752000.0, 2756000.0, 2760000.0, 2764000.0, 2768000.0,
             2772000.0, 2776000.0, 2781000.0, 2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0, 2813000.0, 2818000.0,
             2822000.0, 2826000.0, 2830000.0, 2840000.0, 2850000.0, 2860000.0, 2870000.0, 2880000.0, 2890000.0, 2900000.0, 2910000.0, 2921000.0,
             2931000.0, 2951000.0, 2971000.0, 2991000.0, 3011000.0, 3031000.0, 3052000.0, 3072000.0, 3092000.0, 3112000.0, 3133000.0, 3153000.0,
             3174000.0, 3194000.0, 3215000.0, 3280000.0, 3381000.0, 3488000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2710000.0, 2712000.0, 2715000.0, 2717000.0, 2719000.0, 2721000.0,
             2723000.0, 2725000.0, 2730000.0, 2734000.0, 2738000.0, 2743000.0, 2747000.0, 2751000.0, 2755000.0, 2759000.0, 2764000.0, 2768000.0,
             2772000.0, 2776000.0, 2780000.0, 2784000.0, 2788000.0, 2793000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0, 2813000.0, 2817000.0,
             2821000.0, 2825000.0, 2829000.0, 2839000.0, 2850000.0, 2860000.0, 2870000.0, 2880000.0, 2890000.0, 2900000.0, 2910000.0, 2920000.0,
             2930000.0, 2950000.0, 2971000.0, 2991000.0, 3011000.0, 3031000.0, 3051000.0, 3072000.0, 3092000.0, 3112000.0, 3133000.0, 3153000.0,
             3174000.0, 3194000.0, 3215000.0, 3280000.0, 3381000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2712000.0, 2714000.0, 2716000.0, 2718000.0, 2720000.0,
             2723000.0, 2725000.0, 2729000.0, 2733000.0, 2738000.0, 2742000.0, 2746000.0, 2750000.0, 2755000.0, 2759000.0, 2763000.0, 2767000.0,
             2771000.0, 2775000.0, 2780000.0, 2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2813000.0, 2817000.0,
             2821000.0, 2825000.0, 2829000.0, 2839000.0, 2849000.0, 2859000.0, 2870000.0, 2880000.0, 2890000.0, 2900000.0, 2910000.0, 2920000.0,
             2930000.0, 2950000.0, 2970000.0, 2991000.0, 3011000.0, 3031000.0, 3051000.0, 3071000.0, 3092000.0, 3112000.0, 3132000.0, 3153000.0,
             3173000.0, 3194000.0, 3215000.0, 3280000.0, 3381000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2713000.0, 2715000.0, 2717000.0, 2720000.0,
             2722000.0, 2724000.0, 2728000.0, 2733000.0, 2737000.0, 2741000.0, 2746000.0, 2750000.0, 2754000.0, 2758000.0, 2762000.0, 2767000.0,
             2771000.0, 2775000.0, 2779000.0, 2783000.0, 2787000.0, 2792000.0, 2796000.0, 2800000.0, 2804000.0, 2808000.0, 2812000.0, 2816000.0,
             2820000.0, 2824000.0, 2829000.0, 2839000.0, 2849000.0, 2859000.0, 2869000.0, 2879000.0, 2889000.0, 2900000.0, 2910000.0, 2920000.0,
             2930000.0, 2950000.0, 2970000.0, 2990000.0, 3011000.0, 3031000.0, 3051000.0, 3071000.0, 3092000.0, 3112000.0, 3132000.0, 3153000.0,
             3173000.0, 3194000.0, 3214000.0, 3280000.0, 3381000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2715000.0, 2717000.0, 2719000.0,
             2721000.0, 2723000.0, 2728000.0, 2732000.0, 2736000.0, 2741000.0, 2745000.0, 2749000.0, 2753000.0, 2758000.0, 2762000.0, 2766000.0,
             2770000.0, 2774000.0, 2779000.0, 2783000.0, 2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2808000.0, 2812000.0, 2816000.0,
             2820000.0, 2824000.0, 2828000.0, 2838000.0, 2849000.0, 2859000.0, 2869000.0, 2879000.0, 2889000.0, 2899000.0, 2909000.0, 2919000.0,
             2930000.0, 2950000.0, 2970000.0, 2990000.0, 3010000.0, 3031000.0, 3051000.0, 3071000.0, 3091000.0, 3112000.0, 3132000.0, 3153000.0,
             3173000.0, 3194000.0, 3214000.0, 3280000.0, 3381000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2716000.0, 2718000.0,
             2720000.0, 2723000.0, 2727000.0, 2731000.0, 2736000.0, 2740000.0, 2744000.0, 2748000.0, 2753000.0, 2757000.0, 2761000.0, 2765000.0,
             2770000.0, 2774000.0, 2778000.0, 2782000.0, 2786000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2807000.0, 2811000.0, 2815000.0,
             2819000.0, 2824000.0, 2828000.0, 2838000.0, 2848000.0, 2858000.0, 2868000.0, 2879000.0, 2889000.0, 2899000.0, 2909000.0, 2919000.0,
             2929000.0, 2949000.0, 2970000.0, 2990000.0, 3010000.0, 3030000.0, 3051000.0, 3071000.0, 3091000.0, 3112000.0, 3132000.0, 3152000.0,
             3173000.0, 3194000.0, 3214000.0, 3280000.0, 3381000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2717000.0,
             2720000.0, 2722000.0, 2726000.0, 2731000.0, 2735000.0, 2739000.0, 2744000.0, 2748000.0, 2752000.0, 2756000.0, 2761000.0, 2765000.0,
             2769000.0, 2773000.0, 2777000.0, 2782000.0, 2786000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2807000.0, 2811000.0, 2815000.0,
             2819000.0, 2823000.0, 2827000.0, 2837000.0, 2848000.0, 2858000.0, 2868000.0, 2878000.0, 2888000.0, 2899000.0, 2909000.0, 2919000.0,
             2929000.0, 2949000.0, 2969000.0, 2990000.0, 3010000.0, 3030000.0, 3050000.0, 3071000.0, 3091000.0, 3111000.0, 3132000.0, 3152000.0,
             3173000.0, 3193000.0, 3214000.0, 3280000.0, 3381000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             2719000.0, 2721000.0, 2725000.0, 2730000.0, 2734000.0, 2738000.0, 2743000.0, 2747000.0, 2751000.0, 2756000.0, 2760000.0, 2764000.0,
             2768000.0, 2773000.0, 2777000.0, 2781000.0, 2785000.0, 2789000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2810000.0, 2814000.0,
             2819000.0, 2823000.0, 2827000.0, 2837000.0, 2847000.0, 2858000.0, 2868000.0, 2878000.0, 2888000.0, 2898000.0, 2908000.0, 2919000.0,
             2929000.0, 2949000.0, 2969000.0, 2989000.0, 3010000.0, 3030000.0, 3050000.0, 3071000.0, 3091000.0, 3111000.0, 3132000.0, 3152000.0,
             3173000.0, 3193000.0, 3214000.0, 3280000.0, 3381000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       2720000.0, 2725000.0, 2729000.0, 2733000.0, 2738000.0, 2742000.0, 2746000.0, 2751000.0, 2755000.0, 2759000.0, 2764000.0,
             2768000.0, 2772000.0, 2776000.0, 2781000.0, 2785000.0, 2789000.0, 2793000.0, 2797000.0, 2801000.0, 2806000.0, 2810000.0, 2814000.0,
             2818000.0, 2822000.0, 2826000.0, 2837000.0, 2847000.0, 2857000.0, 2867000.0, 2878000.0, 2888000.0, 2898000.0, 2908000.0, 2918000.0,
             2928000.0, 2949000.0, 2969000.0, 2989000.0, 3009000.0, 3030000.0, 3050000.0, 3070000.0, 3091000.0, 3111000.0, 3132000.0, 3152000.0,
             3173000.0, 3193000.0, 3214000.0, 3280000.0, 3381000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       2723000.0, 2727000.0, 2732000.0, 2736000.0, 2741000.0, 2745000.0, 2749000.0, 2754000.0, 2758000.0, 2762000.0,
             2767000.0, 2771000.0, 2775000.0, 2779000.0, 2784000.0, 2788000.0, 2792000.0, 2796000.0, 2800000.0, 2805000.0, 2809000.0, 2813000.0,
             2817000.0, 2821000.0, 2825000.0, 2836000.0, 2846000.0, 2856000.0, 2867000.0, 2877000.0, 2887000.0, 2897000.0, 2907000.0, 2918000.0,
             2928000.0, 2948000.0, 2968000.0, 2989000.0, 3009000.0, 3029000.0, 3050000.0, 3070000.0, 3090000.0, 3111000.0, 3131000.0, 3152000.0,
             3172000.0, 3193000.0, 3213000.0, 3280000.0, 3380000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       2726000.0, 2730000.0, 2735000.0, 2739000.0, 2743000.0, 2748000.0, 2752000.0, 2757000.0, 2761000.0,
             2765000.0, 2769000.0, 2774000.0, 2778000.0, 2782000.0, 2787000.0, 2791000.0, 2795000.0, 2799000.0, 2803000.0, 2808000.0, 2812000.0,
             2816000.0, 2820000.0, 2824000.0, 2835000.0, 2845000.0, 2855000.0, 2866000.0, 2876000.0, 2886000.0, 2896000.0, 2907000.0, 2917000.0,
             2927000.0, 2947000.0, 2968000.0, 2988000.0, 3008000.0, 3029000.0, 3049000.0, 3069000.0, 3090000.0, 3110000.0, 3131000.0, 3151000.0,
             3172000.0, 3192000.0, 3213000.0, 3280000.0, 3380000.0, 3487000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       2728000.0, 2733000.0, 2737000.0, 2742000.0, 2746000.0, 2751000.0, 2755000.0, 2759000.0,
             2764000.0, 2768000.0, 2772000.0, 2777000.0, 2781000.0, 2785000.0, 2790000.0, 2794000.0, 2798000.0, 2802000.0, 2806000.0, 2811000.0,
             2815000.0, 2819000.0, 2823000.0, 2834000.0, 2844000.0, 2854000.0, 2865000.0, 2875000.0, 2885000.0, 2896000.0, 2906000.0, 2916000.0,
             2926000.0, 2947000.0, 2967000.0, 2987000.0, 3008000.0, 3028000.0, 3049000.0, 3069000.0, 3089000.0, 3110000.0, 3130000.0, 3151000.0,
             3172000.0, 3192000.0, 3213000.0, 3280000.0, 3380000.0, 3486000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       2731000.0, 2735000.0, 2740000.0, 2744000.0, 2749000.0, 2753000.0, 2758000.0,
             2762000.0, 2767000.0, 2771000.0, 2775000.0, 2780000.0, 2784000.0, 2788000.0, 2792000.0, 2797000.0, 2801000.0, 2805000.0, 2809000.0,
             2814000.0, 2818000.0, 2822000.0, 2833000.0, 2843000.0, 2853000.0, 2864000.0, 2874000.0, 2885000.0, 2895000.0, 2905000.0, 2915000.0,
             2926000.0, 2946000.0, 2966000.0, 2987000.0, 3007000.0, 3028000.0, 3048000.0, 3069000.0, 3089000.0, 3109000.0, 3130000.0, 3151000.0,
             3171000.0, 3192000.0, 3212000.0, 3280000.0, 3380000.0, 3486000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2733000.0, 2738000.0, 2743000.0, 2747000.0, 2752000.0, 2756000.0,
             2761000.0, 2765000.0, 2769000.0, 2774000.0, 2778000.0, 2782000.0, 2787000.0, 2791000.0, 2795000.0, 2800000.0, 2804000.0, 2808000.0,
             2812000.0, 2817000.0, 2821000.0, 2831000.0, 2842000.0, 2852000.0, 2863000.0, 2873000.0, 2884000.0, 2894000.0, 2904000.0, 2915000.0,
             2925000.0, 2945000.0, 2966000.0, 2986000.0, 3007000.0, 3027000.0, 3048000.0, 3068000.0, 3089000.0, 3109000.0, 3130000.0, 3150000.0,
             3171000.0, 3191000.0, 3212000.0, 3280000.0, 3380000.0, 3486000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2736000.0, 2741000.0, 2745000.0, 2750000.0, 2754000.0,
             2759000.0, 2763000.0, 2768000.0, 2772000.0, 2777000.0, 2781000.0, 2785000.0, 2790000.0, 2794000.0, 2798000.0, 2803000.0, 2807000.0,
             2811000.0, 2815000.0, 2820000.0, 2830000.0, 2841000.0, 2851000.0, 2862000.0, 2872000.0, 2883000.0, 2893000.0, 2903000.0, 2914000.0,
             2924000.0, 2945000.0, 2965000.0, 2986000.0, 3006000.0, 3027000.0, 3047000.0, 3068000.0, 3088000.0, 3109000.0, 3129000.0, 3150000.0,
             3170000.0, 3191000.0, 3212000.0, 3280000.0, 3379000.0, 3486000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2739000.0, 2743000.0, 2748000.0, 2752000.0,
             2757000.0, 2761000.0, 2766000.0, 2770000.0, 2775000.0, 2779000.0, 2784000.0, 2788000.0, 2792000.0, 2797000.0, 2801000.0, 2805000.0,
             2810000.0, 2814000.0, 2818000.0, 2829000.0, 2840000.0, 2850000.0, 2861000.0, 2871000.0, 2882000.0, 2892000.0, 2902000.0, 2913000.0,
             2923000.0, 2944000.0, 2964000.0, 2985000.0, 3005000.0, 3026000.0, 3046000.0, 3067000.0, 3088000.0, 3108000.0, 3129000.0, 3149000.0,
             3170000.0, 3191000.0, 3211000.0, 3280000.0, 3379000.0, 3485000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2741000.0, 2746000.0, 2750000.0,
             2755000.0, 2760000.0, 2764000.0, 2769000.0, 2773000.0, 2778000.0, 2782000.0, 2786000.0, 2791000.0, 2795000.0, 2800000.0, 2804000.0,
             2808000.0, 2813000.0, 2817000.0, 2828000.0, 2838000.0, 2849000.0, 2860000.0, 2870000.0, 2881000.0, 2891000.0, 2901000.0, 2912000.0,
             2922000.0, 2943000.0, 2964000.0, 2984000.0, 3005000.0, 3025000.0, 3046000.0, 3066000.0, 3087000.0, 3108000.0, 3128000.0, 3149000.0,
             3170000.0, 3190000.0, 3211000.0, 3280000.0, 3379000.0, 3485000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2744000.0, 2748000.0,
             2753000.0, 2758000.0, 2762000.0, 2767000.0, 2771000.0, 2776000.0, 2780000.0, 2785000.0, 2789000.0, 2794000.0, 2798000.0, 2802000.0,
             2807000.0, 2811000.0, 2815000.0, 2826000.0, 2837000.0, 2848000.0, 2858000.0, 2869000.0, 2879000.0, 2890000.0, 2900000.0, 2911000.0,
             2921000.0, 2942000.0, 2963000.0, 2983000.0, 3004000.0, 3025000.0, 3045000.0, 3066000.0, 3086000.0, 3107000.0, 3128000.0, 3148000.0,
             3169000.0, 3190000.0, 3211000.0, 3280000.0, 3378000.0, 3485000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2746000.0,
             2751000.0, 2755000.0, 2760000.0, 2765000.0, 2769000.0, 2774000.0, 2778000.0, 2783000.0, 2787000.0, 2792000.0, 2796000.0, 2801000.0,
             2805000.0, 2810000.0, 2814000.0, 2825000.0, 2836000.0, 2846000.0, 2857000.0, 2868000.0, 2878000.0, 2889000.0, 2899000.0, 2910000.0,
             2920000.0, 2941000.0, 2962000.0, 2983000.0, 3003000.0, 3024000.0, 3045000.0, 3065000.0, 3086000.0, 3106000.0, 3127000.0, 3148000.0,
             3169000.0, 3189000.0, 3210000.0, 3280000.0, 3378000.0, 3485000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             2748000.0, 2753000.0, 2758000.0, 2763000.0, 2767000.0, 2772000.0, 2776000.0, 2781000.0, 2786000.0, 2790000.0, 2795000.0, 2799000.0,
             2803000.0, 2808000.0, 2812000.0, 2823000.0, 2834000.0, 2845000.0, 2856000.0, 2866000.0, 2877000.0, 2888000.0, 2898000.0, 2909000.0,
             2919000.0, 2940000.0, 2961000.0, 2982000.0, 3002000.0, 3023000.0, 3044000.0, 3064000.0, 3085000.0, 3106000.0, 3127000.0, 3147000.0,
             3168000.0, 3189000.0, 3210000.0, 3280000.0, 3378000.0, 3484000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       2751000.0, 2755000.0, 2760000.0, 2765000.0, 2770000.0, 2774000.0, 2779000.0, 2784000.0, 2788000.0, 2793000.0, 2797000.0,
             2802000.0, 2806000.0, 2811000.0, 2822000.0, 2833000.0, 2843000.0, 2854000.0, 2865000.0, 2876000.0, 2886000.0, 2897000.0, 2908000.0,
             2918000.0, 2939000.0, 2960000.0, 2981000.0, 3002000.0, 3022000.0, 3043000.0, 3064000.0, 3085000.0, 3105000.0, 3126000.0, 3147000.0,
             3168000.0, 3188000.0, 3209000.0, 3280000.0, 3377000.0, 3484000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       2753000.0, 2758000.0, 2763000.0, 2767000.0, 2772000.0, 2777000.0, 2781000.0, 2786000.0, 2791000.0, 2795000.0,
             2800000.0, 2804000.0, 2809000.0, 2820000.0, 2831000.0, 2842000.0, 2853000.0, 2864000.0, 2874000.0, 2885000.0, 2896000.0, 2906000.0,
             2917000.0, 2938000.0, 2959000.0, 2980000.0, 3001000.0, 3022000.0, 3042000.0, 3063000.0, 3084000.0, 3105000.0, 3125000.0, 3146000.0,
             3167000.0, 3188000.0, 3209000.0, 3280000.0, 3377000.0, 3484000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       2755000.0, 2760000.0, 2765000.0, 2770000.0, 2775000.0, 2779000.0, 2784000.0, 2789000.0, 2793000.0,
             2798000.0, 2802000.0, 2807000.0, 2818000.0, 2829000.0, 2840000.0, 2851000.0, 2862000.0, 2873000.0, 2884000.0, 2894000.0, 2905000.0,
             2916000.0, 2937000.0, 2958000.0, 2979000.0, 3000000.0, 3021000.0, 3042000.0, 3062000.0, 3083000.0, 3104000.0, 3125000.0, 3146000.0,
             3166000.0, 3187000.0, 3208000.0, 3280000.0, 3377000.0, 3484000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       2757000.0, 2762000.0, 2767000.0, 2772000.0, 2777000.0, 2782000.0, 2786000.0, 2791000.0,
             2796000.0, 2800000.0, 2805000.0, 2816000.0, 2827000.0, 2839000.0, 2850000.0, 2861000.0, 2872000.0, 2882000.0, 2893000.0, 2904000.0,
             2915000.0, 2936000.0, 2957000.0, 2978000.0, 2999000.0, 3020000.0, 3041000.0, 3062000.0, 3082000.0, 3103000.0, 3124000.0, 3145000.0,
             3166000.0, 3187000.0, 3208000.0, 3280000.0, 3376000.0, 3483000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       2760000.0, 2765000.0, 2770000.0, 2774000.0, 2779000.0, 2784000.0, 2789000.0,
             2793000.0, 2798000.0, 2803000.0, 2814000.0, 2826000.0, 2837000.0, 2848000.0, 2859000.0, 2870000.0, 2881000.0, 2892000.0, 2902000.0,
             2913000.0, 2935000.0, 2956000.0, 2977000.0, 2998000.0, 3019000.0, 3040000.0, 3061000.0, 3082000.0, 3102000.0, 3123000.0, 3144000.0,
             3165000.0, 3186000.0, 3207000.0, 3280000.0, 3376000.0, 3483000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2762000.0, 2767000.0, 2772000.0, 2777000.0, 2781000.0, 2786000.0,
             2791000.0, 2796000.0, 2800000.0, 2812000.0, 2824000.0, 2835000.0, 2846000.0, 2857000.0, 2868000.0, 2879000.0, 2890000.0, 2901000.0,
             2912000.0, 2933000.0, 2955000.0, 2976000.0, 2997000.0, 3018000.0, 3039000.0, 3060000.0, 3081000.0, 3102000.0, 3123000.0, 3144000.0,
             3164000.0, 3185000.0, 3206000.0, 3280000.0, 3375000.0, 3483000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2764000.0, 2769000.0, 2774000.0, 2779000.0, 2784000.0,
             2789000.0, 2793000.0, 2798000.0, 2810000.0, 2821000.0, 2833000.0, 2844000.0, 2855000.0, 2867000.0, 2878000.0, 2889000.0, 2900000.0,
             2910000.0, 2932000.0, 2953000.0, 2975000.0, 2996000.0, 3017000.0, 3038000.0, 3059000.0, 3080000.0, 3101000.0, 3122000.0, 3143000.0,
             3164000.0, 3185000.0, 3206000.0, 3280000.0, 3375000.0, 3482000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2766000.0, 2771000.0, 2776000.0, 2781000.0,
             2786000.0, 2791000.0, 2796000.0, 2808000.0, 2819000.0, 2831000.0, 2842000.0, 2854000.0, 2865000.0, 2876000.0, 2887000.0, 2898000.0,
             2909000.0, 2931000.0, 2952000.0, 2973000.0, 2995000.0, 3016000.0, 3037000.0, 3058000.0, 3079000.0, 3100000.0, 3121000.0, 3142000.0,
             3163000.0, 3184000.0, 3205000.0, 3280000.0, 3374000.0, 3482000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2768000.0, 2773000.0, 2778000.0,
             2783000.0, 2788000.0, 2793000.0, 2805000.0, 2817000.0, 2829000.0, 2840000.0, 2852000.0, 2863000.0, 2874000.0, 2885000.0, 2896000.0,
             2907000.0, 2929000.0, 2951000.0, 2972000.0, 2994000.0, 3015000.0, 3036000.0, 3057000.0, 3078000.0, 3099000.0, 3120000.0, 3141000.0,
             3162000.0, 3183000.0, 3204000.0, 3280000.0, 3374000.0, 3481000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2770000.0, 2775000.0,
             2780000.0, 2785000.0, 2790000.0, 2802000.0, 2814000.0, 2826000.0, 2838000.0, 2850000.0, 2861000.0, 2872000.0, 2883000.0, 2895000.0,
             2906000.0, 2928000.0, 2949000.0, 2971000.0, 2992000.0, 3014000.0, 3035000.0, 3056000.0, 3077000.0, 3098000.0, 3119000.0, 3140000.0,
             3162000.0, 3183000.0, 3204000.0, 3280000.0, 3373000.0, 3481000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2772000.0,
             2777000.0, 2782000.0, 2787000.0, 2800000.0, 2812000.0, 2824000.0, 2836000.0, 2847000.0, 2859000.0, 2870000.0, 2882000.0, 2893000.0,
             2904000.0, 2926000.0, 2948000.0, 2969000.0, 2991000.0, 3012000.0, 3034000.0, 3055000.0, 3076000.0, 3097000.0, 3118000.0, 3140000.0,
             3161000.0, 3182000.0, 3203000.0, 3280000.0, 3373000.0, 3480000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             2774000.0, 2779000.0, 2784000.0, 2797000.0, 2809000.0, 2821000.0, 2833000.0, 2845000.0, 2857000.0, 2868000.0, 2880000.0, 2891000.0,
             2902000.0, 2924000.0, 2946000.0, 2968000.0, 2990000.0, 3011000.0, 3033000.0, 3054000.0, 3075000.0, 3096000.0, 3118000.0, 3139000.0,
             3160000.0, 3181000.0, 3202000.0, 3280000.0, 3372000.0, 3480000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       2775000.0, 2781000.0, 2794000.0, 2806000.0, 2819000.0, 2831000.0, 2843000.0, 2854000.0, 2866000.0, 2878000.0, 2889000.0,
             2900000.0, 2923000.0, 2945000.0, 2967000.0, 2988000.0, 3010000.0, 3031000.0, 3053000.0, 3074000.0, 3095000.0, 3117000.0, 3138000.0,
             3159000.0, 3180000.0, 3201000.0, 3280000.0, 3372000.0, 3480000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       2777000.0, 2790000.0, 2803000.0, 2816000.0, 2828000.0, 2840000.0, 2852000.0, 2864000.0, 2875000.0, 2887000.0,
             2898000.0, 2921000.0, 2943000.0, 2965000.0, 2987000.0, 3009000.0, 3030000.0, 3052000.0, 3073000.0, 3094000.0, 3116000.0, 3137000.0,
             3158000.0, 3179000.0, 3201000.0, 3280000.0, 3371000.0, 3479000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       2781000.0, 2795000.0, 2808000.0, 2821000.0, 2833000.0, 2846000.0, 2858000.0, 2870000.0, 2881000.0,
             2893000.0, 2916000.0, 2939000.0, 2961000.0, 2983000.0, 3005000.0, 3027000.0, 3048000.0, 3070000.0, 3091000.0, 3113000.0, 3134000.0,
             3156000.0, 3177000.0, 3198000.0, 3280000.0, 3370000.0, 3478000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       2785000.0, 2799000.0, 2813000.0, 2826000.0, 2838000.0, 2851000.0, 2863000.0, 2875000.0,
             2887000.0, 2910000.0, 2933000.0, 2956000.0, 2979000.0, 3001000.0, 3023000.0, 3045000.0, 3067000.0, 3088000.0, 3110000.0, 3132000.0,
             3153000.0, 3175000.0, 3196000.0, 3280000.0, 3368000.0, 3476000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       2789000.0, 2803000.0, 2817000.0, 2830000.0, 2843000.0, 2856000.0, 2868000.0,
             2880000.0, 2904000.0, 2928000.0, 2951000.0, 2974000.0, 2996000.0, 3019000.0, 3041000.0, 3063000.0, 3085000.0, 3107000.0, 3128000.0,
             3150000.0, 3172000.0, 3193000.0, 3280000.0, 3366000.0, 3475000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2792000.0, 2807000.0, 2821000.0, 2834000.0, 2847000.0, 2860000.0,
             2873000.0, 2898000.0, 2922000.0, 2945000.0, 2969000.0, 2992000.0, 3014000.0, 3037000.0, 3059000.0, 3081000.0, 3103000.0, 3125000.0,
             3147000.0, 3169000.0, 3190000.0, 3280000.0, 3364000.0, 3473000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2795000.0, 2810000.0, 2824000.0, 2838000.0, 2851000.0,
             2864000.0, 2890000.0, 2915000.0, 2939000.0, 2963000.0, 2986000.0, 3009000.0, 3032000.0, 3055000.0, 3077000.0, 3099000.0, 3121000.0,
             3143000.0, 3165000.0, 3187000.0, 3280000.0, 3362000.0, 3471000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2797000.0, 2813000.0, 2827000.0, 2841000.0,
             2855000.0, 2882000.0, 2907000.0, 2932000.0, 2956000.0, 2980000.0, 3004000.0, 3027000.0, 3050000.0, 3072000.0, 3095000.0, 3117000.0,
             3140000.0, 3162000.0, 3184000.0, 3280000.0, 3359000.0, 3469000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2799000.0, 2815000.0, 2830000.0,
             2844000.0, 2872000.0, 2899000.0, 2924000.0, 2949000.0, 2974000.0, 2998000.0, 3021000.0, 3044000.0, 3067000.0, 3090000.0, 3113000.0,
             3135000.0, 3158000.0, 3180000.0, 3280000.0, 3357000.0, 3467000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2801000.0, 2817000.0,
             2832000.0, 2862000.0, 2889000.0, 2916000.0, 2941000.0, 2966000.0, 2991000.0, 3015000.0, 3039000.0, 3062000.0, 3085000.0, 3108000.0,
             3131000.0, 3154000.0, 3176000.0, 3280000.0, 3354000.0, 3465000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2802000.0,
             2819000.0, 2850000.0, 2879000.0, 2906000.0, 2933000.0, 2958000.0, 2984000.0, 3008000.0, 3032000.0, 3056000.0, 3080000.0, 3103000.0,
             3126000.0, 3149000.0, 3172000.0, 3280000.0, 3351000.0, 3462000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             2803000.0, 2836000.0, 2867000.0, 2895000.0, 2923000.0, 2950000.0, 2975000.0, 3001000.0, 3025000.0, 3050000.0, 3073000.0, 3097000.0,
             3121000.0, 3144000.0, 3167000.0, 3280000.0, 3348000.0, 3459000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       2803000.0, 2838000.0, 2870000.0, 2900000.0, 2929000.0, 2957000.0, 2983000.0, 3009000.0, 3035000.0, 3060000.0, 3084000.0,
             3108000.0, 3132000.0, 3156000.0, 3280000.0, 3340000.0, 3453000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       2801000.0, 2838000.0, 2872000.0, 2904000.0, 2934000.0, 2963000.0, 2990000.0, 3017000.0, 3043000.0, 3069000.0,
             3094000.0, 3119000.0, 3143000.0, 3280000.0, 3332000.0, 3446000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       2797000.0, 2837000.0, 2873000.0, 2906000.0, 2937000.0, 2967000.0, 2996000.0, 3023000.0, 3050000.0,
             3077000.0, 3103000.0, 3128000.0, 3280000.0, 3322000.0, 3438000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       2790000.0, 2833000.0, 2871000.0, 2906000.0, 2939000.0, 2970000.0, 3000000.0, 3029000.0,
             3057000.0, 3084000.0, 3110000.0, 3280000.0, 3310000.0, 3429000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       2780000.0, 2826000.0, 2867000.0, 2905000.0, 2939000.0, 2972000.0, 3003000.0,
             3033000.0, 3062000.0, 3090000.0, 3280000.0, 3297000.0, 3418000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0,       0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 2767000.0, 2817000.0,
             2861000.0, 2901000.0, 2938000.0, 2972000.0, 3004000.0, 3036000.0, 3066000.0, 3280000.0, 3282000.0, 3406000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 2750000.0,
             2806000.0, 2853000.0, 2895000.0, 2934000.0, 2970000.0, 3004000.0, 3037000.0, 3280000.0, 3264000.0, 3392000.0},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
             0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 2728000.0,
             2790000.0, 2842000.0, 2887000.0, 2929000.0, 2967000.0, 3003000.0, 3280000.0, 3244000.0, 3377000.0},
            {0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 2701000.0, 2771000.0, 2828000.0, 2877000.0, 2921000.0, 2961000.0, 3280000.0, 3222000.0, 3359000.0},
            {0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,      0.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 0.0, 2666000.0, 2747000.0, 2810000.0, 2864000.0, 2911000.0, 3280000.0, 3195000.0, 3339000.0},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2622000.0, 2718000.0, 2789000.0, 2847000.0, 3280000.0, 3165000.0, 3316000.0},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2564000.0, 2683000.0, 2763000.0, 3280000.0, 3130000.0, 3290000.0},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,       0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2481000.0, 2641000.0, 3280000.0, 3089000.0, 3260000.0},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2335000.0, 3280000.0, 3040000.0, 3226000.0},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3280000.0, 2821000.0, 3085000.0},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3280000.0, 2671000.0, 2998000.0},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,       0.0,      0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3280000.0, 2512000.0, 2906000.0},
        }};

    [[maybe_unused]] constexpr std::array<std::array<Real64, DefaultNumSteamSuperheatedTemps>, DefaultNumSteamSuperheatedPressure>
        DefaultSteamSuperheatedDensityDataTable = {{
            {4.855e-03, 4.837e-03, 4.767e-03, 4.683e-03, 4.601e-03, 4.522e-03, 4.446e-03, 4.373e-03, 4.302e-03, 4.233e-03, 4.167e-03, 4.102e-03,
             4.039e-03, 3.979e-03, 3.920e-03, 3.863e-03, 3.840e-03, 3.818e-03, 3.796e-03, 3.775e-03, 3.753e-03, 3.732e-03, 3.711e-03, 3.691e-03,
             3.670e-03, 3.650e-03, 3.630e-03, 3.610e-03, 3.591e-03, 3.571e-03, 3.562e-03, 3.552e-03, 3.543e-03, 3.533e-03, 3.524e-03, 3.514e-03,
             3.505e-03, 3.496e-03, 3.487e-03, 3.477e-03, 3.468e-03, 3.459e-03, 3.450e-03, 3.441e-03, 3.432e-03, 3.424e-03, 3.415e-03, 3.406e-03,
             3.397e-03, 3.388e-03, 3.380e-03, 3.371e-03, 3.363e-03, 3.354e-03, 3.346e-03, 3.337e-03, 3.329e-03, 3.321e-03, 3.312e-03, 3.304e-03,
             3.296e-03, 3.288e-03, 3.271e-03, 3.255e-03, 3.239e-03, 3.224e-03, 3.208e-03, 3.193e-03, 3.177e-03, 3.162e-03, 3.147e-03, 3.132e-03,
             3.117e-03, 3.103e-03, 3.088e-03, 3.074e-03, 3.060e-03, 3.046e-03, 3.032e-03, 3.018e-03, 3.004e-03, 2.991e-03, 2.977e-03, 2.964e-03,
             2.951e-03, 2.938e-03, 2.925e-03, 2.893e-03, 2.862e-03, 2.831e-03, 2.801e-03, 2.772e-03, 2.743e-03, 2.715e-03, 2.688e-03, 2.661e-03,
             2.634e-03, 2.583e-03, 2.533e-03, 2.486e-03, 2.440e-03, 2.396e-03, 2.353e-03, 2.312e-03, 2.273e-03, 2.234e-03, 2.197e-03, 2.162e-03,
             2.127e-03, 2.093e-03, 2.061e-03, 3.542e-05, 1.833e-03, 1.714e-03},
            {0.0,       5.196e-03, 5.121e-03, 5.031e-03, 4.943e-03, 4.859e-03, 4.777e-03, 4.698e-03, 4.622e-03, 4.548e-03, 4.476e-03, 4.407e-03,
             4.340e-03, 4.274e-03, 4.211e-03, 4.150e-03, 4.126e-03, 4.102e-03, 4.078e-03, 4.055e-03, 4.032e-03, 4.009e-03, 3.987e-03, 3.965e-03,
             3.943e-03, 3.921e-03, 3.899e-03, 3.878e-03, 3.857e-03, 3.836e-03, 3.826e-03, 3.816e-03, 3.806e-03, 3.795e-03, 3.785e-03, 3.775e-03,
             3.765e-03, 3.755e-03, 3.746e-03, 3.736e-03, 3.726e-03, 3.716e-03, 3.707e-03, 3.697e-03, 3.687e-03, 3.678e-03, 3.668e-03, 3.659e-03,
             3.650e-03, 3.640e-03, 3.631e-03, 3.622e-03, 3.612e-03, 3.603e-03, 3.594e-03, 3.585e-03, 3.576e-03, 3.567e-03, 3.558e-03, 3.549e-03,
             3.541e-03, 3.532e-03, 3.514e-03, 3.497e-03, 3.480e-03, 3.463e-03, 3.446e-03, 3.430e-03, 3.413e-03, 3.397e-03, 3.381e-03, 3.365e-03,
             3.349e-03, 3.333e-03, 3.318e-03, 3.302e-03, 3.287e-03, 3.272e-03, 3.257e-03, 3.242e-03, 3.228e-03, 3.213e-03, 3.198e-03, 3.184e-03,
             3.170e-03, 3.156e-03, 3.142e-03, 3.108e-03, 3.074e-03, 3.041e-03, 3.009e-03, 2.978e-03, 2.947e-03, 2.917e-03, 2.887e-03, 2.858e-03,
             2.830e-03, 2.775e-03, 2.722e-03, 2.671e-03, 2.621e-03, 2.574e-03, 2.528e-03, 2.484e-03, 2.442e-03, 2.400e-03, 2.361e-03, 2.322e-03,
             2.285e-03, 2.249e-03, 2.214e-03, 3.542e-05, 1.969e-03, 1.841e-03},
            {0.0,       0.0,       6.802e-03, 6.681e-03, 6.565e-03, 6.453e-03, 6.344e-03, 6.239e-03, 6.138e-03, 6.040e-03, 5.944e-03, 5.852e-03,
             5.763e-03, 5.676e-03, 5.592e-03, 5.511e-03, 5.479e-03, 5.447e-03, 5.416e-03, 5.385e-03, 5.355e-03, 5.324e-03, 5.295e-03, 5.265e-03,
             5.236e-03, 5.207e-03, 5.178e-03, 5.150e-03, 5.122e-03, 5.095e-03, 5.081e-03, 5.067e-03, 5.054e-03, 5.040e-03, 5.027e-03, 5.014e-03,
             5.000e-03, 4.987e-03, 4.974e-03, 4.961e-03, 4.948e-03, 4.935e-03, 4.922e-03, 4.909e-03, 4.897e-03, 4.884e-03, 4.871e-03, 4.859e-03,
             4.846e-03, 4.834e-03, 4.822e-03, 4.809e-03, 4.797e-03, 4.785e-03, 4.773e-03, 4.761e-03, 4.749e-03, 4.737e-03, 4.725e-03, 4.714e-03,
             4.702e-03, 4.690e-03, 4.667e-03, 4.644e-03, 4.621e-03, 4.599e-03, 4.577e-03, 4.555e-03, 4.533e-03, 4.511e-03, 4.490e-03, 4.468e-03,
             4.447e-03, 4.427e-03, 4.406e-03, 4.385e-03, 4.365e-03, 4.345e-03, 4.325e-03, 4.306e-03, 4.286e-03, 4.267e-03, 4.247e-03, 4.228e-03,
             4.210e-03, 4.191e-03, 4.172e-03, 4.127e-03, 4.082e-03, 4.039e-03, 3.996e-03, 3.954e-03, 3.913e-03, 3.873e-03, 3.834e-03, 3.796e-03,
             3.758e-03, 3.685e-03, 3.614e-03, 3.546e-03, 3.481e-03, 3.418e-03, 3.357e-03, 3.299e-03, 3.242e-03, 3.188e-03, 3.135e-03, 3.084e-03,
             3.034e-03, 2.986e-03, 2.940e-03, 3.542e-05, 2.615e-03, 2.445e-03},
            {0.0,       0.0,       0.0,       9.407e-03, 9.243e-03, 9.084e-03, 8.931e-03, 8.783e-03, 8.640e-03, 8.502e-03, 8.368e-03, 8.238e-03,
             8.113e-03, 7.991e-03, 7.872e-03, 7.757e-03, 7.712e-03, 7.668e-03, 7.624e-03, 7.580e-03, 7.537e-03, 7.495e-03, 7.453e-03, 7.411e-03,
             7.370e-03, 7.330e-03, 7.289e-03, 7.250e-03, 7.210e-03, 7.172e-03, 7.152e-03, 7.133e-03, 7.114e-03, 7.095e-03, 7.076e-03, 7.057e-03,
             7.039e-03, 7.020e-03, 7.002e-03, 6.983e-03, 6.965e-03, 6.947e-03, 6.929e-03, 6.911e-03, 6.893e-03, 6.875e-03, 6.857e-03, 6.840e-03,
             6.822e-03, 6.805e-03, 6.787e-03, 6.770e-03, 6.753e-03, 6.736e-03, 6.719e-03, 6.702e-03, 6.685e-03, 6.668e-03, 6.651e-03, 6.635e-03,
             6.618e-03, 6.602e-03, 6.569e-03, 6.537e-03, 6.505e-03, 6.473e-03, 6.442e-03, 6.411e-03, 6.380e-03, 6.350e-03, 6.320e-03, 6.290e-03,
             6.260e-03, 6.231e-03, 6.202e-03, 6.173e-03, 6.144e-03, 6.116e-03, 6.088e-03, 6.060e-03, 6.033e-03, 6.006e-03, 5.979e-03, 5.952e-03,
             5.925e-03, 5.899e-03, 5.873e-03, 5.809e-03, 5.746e-03, 5.685e-03, 5.625e-03, 5.566e-03, 5.508e-03, 5.452e-03, 5.397e-03, 5.342e-03,
             5.289e-03, 5.186e-03, 5.087e-03, 4.992e-03, 4.900e-03, 4.811e-03, 4.726e-03, 4.643e-03, 4.564e-03, 4.487e-03, 4.412e-03, 4.340e-03,
             4.271e-03, 4.203e-03, 4.138e-03, 3.542e-05, 3.680e-03, 3.442e-03},
            {0.0,       0.0,       0.0,       0.0,       1.284e-02, 1.262e-02, 1.241e-02, 1.220e-02, 1.200e-02, 1.181e-02, 1.162e-02, 1.144e-02,
             1.127e-02, 1.110e-02, 1.093e-02, 1.078e-02, 1.071e-02, 1.065e-02, 1.059e-02, 1.053e-02, 1.047e-02, 1.041e-02, 1.035e-02, 1.029e-02,
             1.024e-02, 1.018e-02, 1.012e-02, 1.007e-02, 1.001e-02, 9.961e-03, 9.934e-03, 9.907e-03, 9.881e-03, 9.855e-03, 9.828e-03, 9.802e-03,
             9.776e-03, 9.750e-03, 9.725e-03, 9.699e-03, 9.674e-03, 9.649e-03, 9.623e-03, 9.598e-03, 9.574e-03, 9.549e-03, 9.524e-03, 9.500e-03,
             9.475e-03, 9.451e-03, 9.427e-03, 9.403e-03, 9.379e-03, 9.355e-03, 9.332e-03, 9.308e-03, 9.285e-03, 9.261e-03, 9.238e-03, 9.215e-03,
             9.192e-03, 9.170e-03, 9.124e-03, 9.079e-03, 9.035e-03, 8.991e-03, 8.947e-03, 8.904e-03, 8.862e-03, 8.819e-03, 8.777e-03, 8.736e-03,
             8.695e-03, 8.654e-03, 8.614e-03, 8.574e-03, 8.534e-03, 8.495e-03, 8.456e-03, 8.417e-03, 8.379e-03, 8.341e-03, 8.304e-03, 8.267e-03,
             8.230e-03, 8.193e-03, 8.157e-03, 8.068e-03, 7.981e-03, 7.896e-03, 7.812e-03, 7.731e-03, 7.651e-03, 7.572e-03, 7.495e-03, 7.420e-03,
             7.346e-03, 7.203e-03, 7.065e-03, 6.933e-03, 6.805e-03, 6.682e-03, 6.563e-03, 6.449e-03, 6.338e-03, 6.231e-03, 6.128e-03, 6.028e-03,
             5.931e-03, 5.838e-03, 5.747e-03, 3.542e-05, 5.111e-03, 4.781e-03},
            {0.0,       0.0,       0.0,       0.0,       0.0,       1.731e-02, 1.702e-02, 1.674e-02, 1.646e-02, 1.620e-02, 1.594e-02, 1.570e-02,
             1.546e-02, 1.522e-02, 1.500e-02, 1.478e-02, 1.469e-02, 1.461e-02, 1.452e-02, 1.444e-02, 1.436e-02, 1.428e-02, 1.420e-02, 1.412e-02,
             1.404e-02, 1.396e-02, 1.389e-02, 1.381e-02, 1.374e-02, 1.366e-02, 1.362e-02, 1.359e-02, 1.355e-02, 1.352e-02, 1.348e-02, 1.344e-02,
             1.341e-02, 1.337e-02, 1.334e-02, 1.330e-02, 1.327e-02, 1.323e-02, 1.320e-02, 1.316e-02, 1.313e-02, 1.310e-02, 1.306e-02, 1.303e-02,
             1.300e-02, 1.296e-02, 1.293e-02, 1.290e-02, 1.286e-02, 1.283e-02, 1.280e-02, 1.277e-02, 1.273e-02, 1.270e-02, 1.267e-02, 1.264e-02,
             1.261e-02, 1.258e-02, 1.251e-02, 1.245e-02, 1.239e-02, 1.233e-02, 1.227e-02, 1.221e-02, 1.215e-02, 1.210e-02, 1.204e-02, 1.198e-02,
             1.192e-02, 1.187e-02, 1.181e-02, 1.176e-02, 1.170e-02, 1.165e-02, 1.160e-02, 1.154e-02, 1.149e-02, 1.144e-02, 1.139e-02, 1.134e-02,
             1.129e-02, 1.124e-02, 1.119e-02, 1.107e-02, 1.095e-02, 1.083e-02, 1.071e-02, 1.060e-02, 1.049e-02, 1.038e-02, 1.028e-02, 1.018e-02,
             1.007e-02, 9.879e-03, 9.690e-03, 9.508e-03, 9.333e-03, 9.164e-03, 9.001e-03, 8.844e-03, 8.692e-03, 8.546e-03, 8.404e-03, 8.267e-03,
             8.134e-03, 8.006e-03, 7.881e-03, 3.542e-05, 7.009e-03, 6.556e-03},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       2.307e-02, 2.269e-02, 2.232e-02, 2.196e-02, 2.161e-02, 2.128e-02,
             2.095e-02, 2.063e-02, 2.033e-02, 2.003e-02, 1.991e-02, 1.980e-02, 1.968e-02, 1.957e-02, 1.946e-02, 1.935e-02, 1.924e-02, 1.913e-02,
             1.903e-02, 1.892e-02, 1.882e-02, 1.872e-02, 1.862e-02, 1.851e-02, 1.846e-02, 1.842e-02, 1.837e-02, 1.832e-02, 1.827e-02, 1.822e-02,
             1.817e-02, 1.812e-02, 1.808e-02, 1.803e-02, 1.798e-02, 1.793e-02, 1.789e-02, 1.784e-02, 1.779e-02, 1.775e-02, 1.770e-02, 1.766e-02,
             1.761e-02, 1.757e-02, 1.752e-02, 1.748e-02, 1.743e-02, 1.739e-02, 1.734e-02, 1.730e-02, 1.726e-02, 1.721e-02, 1.717e-02, 1.713e-02,
             1.708e-02, 1.704e-02, 1.696e-02, 1.687e-02, 1.679e-02, 1.671e-02, 1.663e-02, 1.655e-02, 1.647e-02, 1.639e-02, 1.631e-02, 1.624e-02,
             1.616e-02, 1.608e-02, 1.601e-02, 1.593e-02, 1.586e-02, 1.579e-02, 1.572e-02, 1.564e-02, 1.557e-02, 1.550e-02, 1.543e-02, 1.536e-02,
             1.530e-02, 1.523e-02, 1.516e-02, 1.499e-02, 1.483e-02, 1.467e-02, 1.452e-02, 1.437e-02, 1.422e-02, 1.407e-02, 1.393e-02, 1.379e-02,
             1.365e-02, 1.339e-02, 1.313e-02, 1.288e-02, 1.265e-02, 1.242e-02, 1.220e-02, 1.198e-02, 1.178e-02, 1.158e-02, 1.139e-02, 1.120e-02,
             1.102e-02, 1.085e-02, 1.068e-02, 3.542e-05, 9.498e-03, 8.884e-03},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       3.042e-02, 2.992e-02, 2.943e-02, 2.897e-02, 2.851e-02,
             2.808e-02, 2.765e-02, 2.724e-02, 2.684e-02, 2.669e-02, 2.653e-02, 2.638e-02, 2.623e-02, 2.608e-02, 2.593e-02, 2.579e-02, 2.564e-02,
             2.550e-02, 2.536e-02, 2.522e-02, 2.508e-02, 2.494e-02, 2.481e-02, 2.474e-02, 2.468e-02, 2.461e-02, 2.454e-02, 2.448e-02, 2.441e-02,
             2.435e-02, 2.428e-02, 2.422e-02, 2.416e-02, 2.409e-02, 2.403e-02, 2.397e-02, 2.391e-02, 2.384e-02, 2.378e-02, 2.372e-02, 2.366e-02,
             2.360e-02, 2.354e-02, 2.348e-02, 2.342e-02, 2.336e-02, 2.330e-02, 2.324e-02, 2.318e-02, 2.312e-02, 2.306e-02, 2.301e-02, 2.295e-02,
             2.289e-02, 2.284e-02, 2.272e-02, 2.261e-02, 2.250e-02, 2.239e-02, 2.228e-02, 2.217e-02, 2.207e-02, 2.196e-02, 2.186e-02, 2.175e-02,
             2.165e-02, 2.155e-02, 2.145e-02, 2.135e-02, 2.125e-02, 2.115e-02, 2.106e-02, 2.096e-02, 2.087e-02, 2.077e-02, 2.068e-02, 2.059e-02,
             2.049e-02, 2.040e-02, 2.031e-02, 2.009e-02, 1.987e-02, 1.966e-02, 1.945e-02, 1.925e-02, 1.905e-02, 1.885e-02, 1.866e-02, 1.848e-02,
             1.829e-02, 1.794e-02, 1.759e-02, 1.726e-02, 1.694e-02, 1.664e-02, 1.634e-02, 1.606e-02, 1.578e-02, 1.552e-02, 1.526e-02, 1.501e-02,
             1.477e-02, 1.453e-02, 1.431e-02, 3.542e-05, 1.273e-02, 1.190e-02},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       3.967e-02, 3.903e-02, 3.841e-02, 3.781e-02,
             3.723e-02, 3.666e-02, 3.612e-02, 3.559e-02, 3.538e-02, 3.518e-02, 3.497e-02, 3.477e-02, 3.457e-02, 3.438e-02, 3.419e-02, 3.399e-02,
             3.380e-02, 3.362e-02, 3.343e-02, 3.325e-02, 3.307e-02, 3.289e-02, 3.280e-02, 3.271e-02, 3.262e-02, 3.254e-02, 3.245e-02, 3.236e-02,
             3.228e-02, 3.219e-02, 3.211e-02, 3.202e-02, 3.194e-02, 3.186e-02, 3.177e-02, 3.169e-02, 3.161e-02, 3.153e-02, 3.144e-02, 3.136e-02,
             3.128e-02, 3.120e-02, 3.112e-02, 3.104e-02, 3.096e-02, 3.089e-02, 3.081e-02, 3.073e-02, 3.065e-02, 3.058e-02, 3.050e-02, 3.042e-02,
             3.035e-02, 3.027e-02, 3.012e-02, 2.997e-02, 2.983e-02, 2.968e-02, 2.954e-02, 2.939e-02, 2.925e-02, 2.911e-02, 2.897e-02, 2.884e-02,
             2.870e-02, 2.857e-02, 2.843e-02, 2.830e-02, 2.817e-02, 2.804e-02, 2.791e-02, 2.778e-02, 2.766e-02, 2.753e-02, 2.741e-02, 2.729e-02,
             2.716e-02, 2.704e-02, 2.692e-02, 2.663e-02, 2.634e-02, 2.606e-02, 2.579e-02, 2.552e-02, 2.525e-02, 2.499e-02, 2.474e-02, 2.449e-02,
             2.425e-02, 2.377e-02, 2.332e-02, 2.288e-02, 2.246e-02, 2.205e-02, 2.166e-02, 2.128e-02, 2.092e-02, 2.057e-02, 2.022e-02, 1.989e-02,
             1.957e-02, 1.927e-02, 1.897e-02, 3.542e-05, 1.687e-02, 1.578e-02},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       5.124e-02, 5.042e-02, 4.963e-02,
             4.887e-02, 4.812e-02, 4.741e-02, 4.671e-02, 4.644e-02, 4.617e-02, 4.590e-02, 4.564e-02, 4.537e-02, 4.512e-02, 4.486e-02, 4.461e-02,
             4.436e-02, 4.412e-02, 4.387e-02, 4.363e-02, 4.340e-02, 4.316e-02, 4.304e-02, 4.293e-02, 4.281e-02, 4.270e-02, 4.258e-02, 4.247e-02,
             4.236e-02, 4.225e-02, 4.213e-02, 4.202e-02, 4.191e-02, 4.180e-02, 4.169e-02, 4.158e-02, 4.148e-02, 4.137e-02, 4.126e-02, 4.116e-02,
             4.105e-02, 4.094e-02, 4.084e-02, 4.073e-02, 4.063e-02, 4.053e-02, 4.043e-02, 4.032e-02, 4.022e-02, 4.012e-02, 4.002e-02, 3.992e-02,
             3.982e-02, 3.972e-02, 3.952e-02, 3.933e-02, 3.914e-02, 3.895e-02, 3.876e-02, 3.857e-02, 3.838e-02, 3.820e-02, 3.802e-02, 3.784e-02,
             3.766e-02, 3.748e-02, 3.731e-02, 3.713e-02, 3.696e-02, 3.679e-02, 3.662e-02, 3.646e-02, 3.629e-02, 3.613e-02, 3.596e-02, 3.580e-02,
             3.564e-02, 3.548e-02, 3.533e-02, 3.494e-02, 3.456e-02, 3.419e-02, 3.383e-02, 3.348e-02, 3.313e-02, 3.279e-02, 3.246e-02, 3.213e-02,
             3.181e-02, 3.119e-02, 3.059e-02, 3.002e-02, 2.947e-02, 2.893e-02, 2.842e-02, 2.792e-02, 2.744e-02, 2.698e-02, 2.653e-02, 2.610e-02,
             2.568e-02, 2.528e-02, 2.488e-02, 3.542e-05, 2.213e-02, 2.070e-02},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       6.556e-02, 6.453e-02,
             6.353e-02, 6.256e-02, 6.163e-02, 6.072e-02, 6.036e-02, 6.001e-02, 5.966e-02, 5.932e-02, 5.898e-02, 5.864e-02, 5.831e-02, 5.799e-02,
             5.766e-02, 5.734e-02, 5.702e-02, 5.671e-02, 5.640e-02, 5.610e-02, 5.594e-02, 5.579e-02, 5.564e-02, 5.549e-02, 5.535e-02, 5.520e-02,
             5.505e-02, 5.490e-02, 5.476e-02, 5.461e-02, 5.447e-02, 5.433e-02, 5.419e-02, 5.404e-02, 5.390e-02, 5.376e-02, 5.362e-02, 5.349e-02,
             5.335e-02, 5.321e-02, 5.307e-02, 5.294e-02, 5.280e-02, 5.267e-02, 5.254e-02, 5.240e-02, 5.227e-02, 5.214e-02, 5.201e-02, 5.188e-02,
             5.175e-02, 5.162e-02, 5.136e-02, 5.111e-02, 5.086e-02, 5.061e-02, 5.036e-02, 5.012e-02, 4.988e-02, 4.964e-02, 4.940e-02, 4.917e-02,
             4.894e-02, 4.871e-02, 4.848e-02, 4.825e-02, 4.803e-02, 4.781e-02, 4.759e-02, 4.737e-02, 4.716e-02, 4.694e-02, 4.673e-02, 4.652e-02,
             4.632e-02, 4.611e-02, 4.591e-02, 4.540e-02, 4.491e-02, 4.443e-02, 4.396e-02, 4.350e-02, 4.305e-02, 4.261e-02, 4.218e-02, 4.175e-02,
             4.134e-02, 4.053e-02, 3.975e-02, 3.901e-02, 3.829e-02, 3.759e-02, 3.693e-02, 3.628e-02, 3.566e-02, 3.506e-02, 3.448e-02, 3.391e-02,
             3.337e-02, 3.284e-02, 3.233e-02, 3.542e-05, 2.875e-02, 2.689e-02},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       8.315e-02,
             8.185e-02, 8.060e-02, 7.939e-02, 7.821e-02, 7.775e-02, 7.730e-02, 7.685e-02, 7.641e-02, 7.597e-02, 7.553e-02, 7.511e-02, 7.468e-02,
             7.426e-02, 7.385e-02, 7.344e-02, 7.304e-02, 7.264e-02, 7.224e-02, 7.205e-02, 7.185e-02, 7.166e-02, 7.147e-02, 7.128e-02, 7.108e-02,
             7.090e-02, 7.071e-02, 7.052e-02, 7.033e-02, 7.015e-02, 6.996e-02, 6.978e-02, 6.960e-02, 6.942e-02, 6.923e-02, 6.906e-02, 6.888e-02,
             6.870e-02, 6.852e-02, 6.835e-02, 6.817e-02, 6.800e-02, 6.782e-02, 6.765e-02, 6.748e-02, 6.731e-02, 6.714e-02, 6.697e-02, 6.680e-02,
             6.664e-02, 6.647e-02, 6.614e-02, 6.581e-02, 6.549e-02, 6.517e-02, 6.485e-02, 6.454e-02, 6.423e-02, 6.392e-02, 6.361e-02, 6.331e-02,
             6.301e-02, 6.272e-02, 6.242e-02, 6.213e-02, 6.185e-02, 6.156e-02, 6.128e-02, 6.100e-02, 6.072e-02, 6.044e-02, 6.017e-02, 5.990e-02,
             5.963e-02, 5.937e-02, 5.911e-02, 5.846e-02, 5.783e-02, 5.721e-02, 5.660e-02, 5.601e-02, 5.543e-02, 5.486e-02, 5.430e-02, 5.375e-02,
             5.322e-02, 5.218e-02, 5.118e-02, 5.022e-02, 4.929e-02, 4.840e-02, 4.754e-02, 4.671e-02, 4.591e-02, 4.513e-02, 4.438e-02, 4.366e-02,
             4.296e-02, 4.228e-02, 4.162e-02, 3.542e-05, 3.701e-02, 3.462e-02},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.10460,   0.10290,   0.10140,   9.988e-02, 9.929e-02, 9.871e-02, 9.813e-02, 9.757e-02, 9.700e-02, 9.645e-02, 9.590e-02, 9.536e-02,
             9.482e-02, 9.430e-02, 9.377e-02, 9.325e-02, 9.274e-02, 9.224e-02, 9.199e-02, 9.174e-02, 9.149e-02, 9.124e-02, 9.100e-02, 9.075e-02,
             9.051e-02, 9.027e-02, 9.003e-02, 8.979e-02, 8.955e-02, 8.932e-02, 8.908e-02, 8.885e-02, 8.862e-02, 8.839e-02, 8.816e-02, 8.793e-02,
             8.770e-02, 8.747e-02, 8.725e-02, 8.703e-02, 8.680e-02, 8.658e-02, 8.636e-02, 8.614e-02, 8.592e-02, 8.571e-02, 8.549e-02, 8.528e-02,
             8.506e-02, 8.485e-02, 8.443e-02, 8.401e-02, 8.360e-02, 8.319e-02, 8.278e-02, 8.238e-02, 8.198e-02, 8.159e-02, 8.120e-02, 8.081e-02,
             8.043e-02, 8.005e-02, 7.968e-02, 7.931e-02, 7.894e-02, 7.857e-02, 7.821e-02, 7.786e-02, 7.750e-02, 7.715e-02, 7.680e-02, 7.646e-02,
             7.611e-02, 7.578e-02, 7.544e-02, 7.461e-02, 7.380e-02, 7.301e-02, 7.224e-02, 7.148e-02, 7.074e-02, 7.001e-02, 6.930e-02, 6.860e-02,
             6.792e-02, 6.659e-02, 6.532e-02, 6.409e-02, 6.291e-02, 6.177e-02, 6.067e-02, 5.961e-02, 5.859e-02, 5.760e-02, 5.664e-02, 5.572e-02,
             5.482e-02, 5.395e-02, 5.312e-02, 3.542e-05, 4.724e-02, 4.418e-02},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.13040,   0.12840,   0.12650,   0.12580,   0.125,     0.12430,   0.12360,   0.12290,   0.12220,   0.12150,   0.12080,
             0.12010,   0.11940,   0.11870,   0.11810,   0.11740,   0.11680,   0.11650,   0.11620,   0.11580,   0.11550,   0.11520,   0.11490,
             0.11460,   0.11430,   0.114,     0.11370,   0.11340,   0.11310,   0.11280,   0.11250,   0.11220,   0.11190,   0.11160,   0.11130,
             0.111,     0.11080,   0.11050,   0.11020,   0.10990,   0.10960,   0.10930,   0.10910,   0.10880,   0.10850,   0.10820,   0.108,
             0.10770,   0.10740,   0.10690,   0.10640,   0.10580,   0.10530,   0.10480,   0.10430,   0.10380,   0.10330,   0.10280,   0.10230,
             0.10180,   0.10130,   0.10090,   0.10040,   9.993e-02, 9.946e-02, 9.901e-02, 9.855e-02, 9.810e-02, 9.766e-02, 9.722e-02, 9.678e-02,
             9.635e-02, 9.592e-02, 9.549e-02, 9.444e-02, 9.342e-02, 9.242e-02, 9.144e-02, 9.048e-02, 8.954e-02, 8.862e-02, 8.771e-02, 8.683e-02,
             8.597e-02, 8.429e-02, 8.267e-02, 8.112e-02, 7.962e-02, 7.818e-02, 7.678e-02, 7.544e-02, 7.415e-02, 7.289e-02, 7.168e-02, 7.051e-02,
             6.938e-02, 6.828e-02, 6.722e-02, 3.542e-05, 5.978e-02, 5.591e-02},
            {0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0,
             0.0,       0.0,       0.16150,   0.159,     0.15810,   0.15710,   0.15620,   0.15530,   0.15440,   0.15350,   0.15260,   0.15180,
             0.15090,   0.15,      0.14920,   0.14840,   0.14760,   0.14670,   0.14630,   0.14590,   0.14550,   0.14520,   0.14480,   0.14440,
             0.144,     0.14360,   0.14320,   0.14280,   0.14250,   0.14210,   0.14170,   0.14130,   0.141,     0.14060,   0.14020,   0.13990,
             0.13950,   0.13910,   0.13880,   0.13840,   0.13810,   0.13770,   0.13730,   0.137,     0.13660,   0.13630,   0.136,     0.13560,
             0.13530,   0.13490,   0.13430,   0.13360,   0.13290,   0.13230,   0.13160,   0.131,     0.13040,   0.12970,   0.12910,   0.12850,
             0.12790,   0.12730,   0.12670,   0.12610,   0.12550,   0.12490,   0.12430,   0.12380,   0.12320,   0.12260,   0.12210,   0.12150,
             0.121,     0.12050,   0.11990,   0.11860,   0.11730,   0.11610,   0.11480,   0.11360,   0.11240,   0.11130,   0.11010,   0.109,
             0.10790,   0.10580,   0.10380,   0.10190,   9.997e-02, 9.816e-02, 9.641e-02, 9.473e-02, 9.310e-02, 9.152e-02, 9.000e-02, 8.853e-02,
             8.711e-02, 8.573e-02, 8.440e-02, 3.542e-05, 7.505e-02, 7.019e-02},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,       0.0,      0.0,     0.0,     0.0,
             0.0,     0.0,     0.19840, 0.19720, 0.19610, 0.19490, 0.19370, 0.19260,   0.19150,   0.19040,  0.18930, 0.18820, 0.18720,
             0.18610, 0.18510, 0.184,   0.183,   0.18250, 0.182,   0.18150, 0.181,     0.18050,   0.18,     0.17960, 0.17910, 0.17860,
             0.17810, 0.17760, 0.17720, 0.17670, 0.17620, 0.17580, 0.17530, 0.17480,   0.17440,   0.17390,  0.17350, 0.173,   0.17260,
             0.17210, 0.17170, 0.17120, 0.17080, 0.17040, 0.16990, 0.16950, 0.16910,   0.16870,   0.16820,  0.16740, 0.16660, 0.16570,
             0.16490, 0.16410, 0.16330, 0.16250, 0.16170, 0.16090, 0.16020, 0.15940,   0.15870,   0.15790,  0.15720, 0.15640, 0.15570,
             0.155,   0.15430, 0.15360, 0.15290, 0.15220, 0.15150, 0.15080, 0.15010,   0.14950,   0.14780,  0.14620, 0.14460, 0.14310,
             0.14160, 0.14010, 0.13870, 0.13730, 0.13590, 0.13450, 0.13190, 0.12940,   0.12690,   0.12460,  0.12230, 0.12010, 0.118,
             0.116,   0.11410, 0.11220, 0.11030, 0.10850, 0.10680, 0.10520, 3.542e-05, 9.352e-02, 8.746e-02},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,      0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.21510, 0.21380, 0.21250, 0.21130, 0.21,    0.20880,   0.20760, 0.20640,  0.20520, 0.204,   0.20290, 0.20180, 0.20060, 0.19950,
             0.199,   0.19840, 0.19790, 0.19730, 0.19680, 0.19630, 0.19570,   0.19520, 0.19470,  0.19420, 0.19360, 0.19310, 0.19260, 0.19210, 0.19160,
             0.19110, 0.19060, 0.19010, 0.18960, 0.18910, 0.18860, 0.18810,   0.18760, 0.18720,  0.18670, 0.18620, 0.18570, 0.18520, 0.18480, 0.18430,
             0.18380, 0.18340, 0.18250, 0.18150, 0.18060, 0.17980, 0.17890,   0.178,   0.17710,  0.17630, 0.17540, 0.17460, 0.17380, 0.17290, 0.17210,
             0.17130, 0.17050, 0.16970, 0.16890, 0.16820, 0.16740, 0.16660,   0.16590, 0.16510,  0.16440, 0.16360, 0.16290, 0.16110, 0.15940, 0.15770,
             0.156,   0.15430, 0.15270, 0.15110, 0.14960, 0.14810, 0.14660,   0.14370, 0.141,    0.13830, 0.13580, 0.13330, 0.13090, 0.12860, 0.12640,
             0.12430, 0.12220, 0.12020, 0.11830, 0.11640, 0.11460, 3.542e-05, 0.10190, 9.531e-02},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.23290, 0.23150, 0.23010, 0.22870, 0.22740,   0.22610, 0.22480, 0.22350, 0.22220, 0.221,   0.21970, 0.21850, 0.21730,
             0.21670, 0.21610, 0.21550, 0.21490, 0.21430, 0.21370, 0.21310,   0.21260, 0.212,   0.21140, 0.21090, 0.21030, 0.20970, 0.20920, 0.20860,
             0.20810, 0.20750, 0.207,   0.20640, 0.20590, 0.20540, 0.20480,   0.20430, 0.20380, 0.20330, 0.20270, 0.20220, 0.20170, 0.20120, 0.20070,
             0.20020, 0.19970, 0.19870, 0.19770, 0.19670, 0.19570, 0.19480,   0.19380, 0.19290, 0.19190, 0.191,   0.19010, 0.18920, 0.18830, 0.18740,
             0.18650, 0.18560, 0.18480, 0.18390, 0.18310, 0.18220, 0.18140,   0.18060, 0.17980, 0.179,   0.17820, 0.17740, 0.17540, 0.17350, 0.17160,
             0.16980, 0.168,   0.16630, 0.16450, 0.16290, 0.16120, 0.15960,   0.15650, 0.15350, 0.15060, 0.14780, 0.14510, 0.14250, 0.14,    0.13760,
             0.13530, 0.133,   0.13090, 0.12880, 0.12670, 0.12480, 3.542e-05, 0.11090, 0.1037},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.25180, 0.25030, 0.24890, 0.24740,   0.246,   0.24450, 0.24310, 0.24170, 0.24040, 0.239,   0.23770, 0.23640,
             0.23570, 0.23510, 0.23440, 0.23380, 0.23310, 0.23250, 0.23190,   0.23120, 0.23060, 0.23,    0.22940, 0.22880, 0.22810, 0.22750, 0.22690,
             0.22630, 0.22570, 0.22510, 0.22460, 0.224,   0.22340, 0.22280,   0.22220, 0.22160, 0.22110, 0.22050, 0.21990, 0.21940, 0.21880, 0.21830,
             0.21770, 0.21720, 0.21610, 0.215,   0.21390, 0.21290, 0.21180,   0.21080, 0.20970, 0.20870, 0.20770, 0.20670, 0.20570, 0.20480, 0.20380,
             0.20280, 0.20190, 0.201,   0.2,     0.19910, 0.19820, 0.19730,   0.19640, 0.19550, 0.19460, 0.19370, 0.19290, 0.19080, 0.18870, 0.18660,
             0.18470, 0.18270, 0.18080, 0.17890, 0.17710, 0.17530, 0.17360,   0.17020, 0.16690, 0.16370, 0.16070, 0.15780, 0.155,   0.15230, 0.14960,
             0.14710, 0.14470, 0.14230, 0.14,    0.13780, 0.13560, 3.542e-05, 0.12060, 0.1128},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.27210, 0.27050, 0.26890,   0.26730, 0.26580, 0.26420, 0.26270, 0.26120, 0.25970, 0.25830, 0.25680,
             0.25610, 0.25540, 0.25470, 0.254,   0.25330, 0.25260, 0.25190,   0.25130, 0.25060, 0.24990, 0.24920, 0.24860, 0.24790, 0.24720, 0.24660,
             0.24590, 0.24530, 0.24460, 0.244,   0.24330, 0.24270, 0.24210,   0.24140, 0.24080, 0.24020, 0.23960, 0.239,   0.23840, 0.23770, 0.23710,
             0.23650, 0.23590, 0.23480, 0.23360, 0.23240, 0.23130, 0.23010,   0.229,   0.22790, 0.22680, 0.22570, 0.22460, 0.22350, 0.22250, 0.22140,
             0.22040, 0.21930, 0.21830, 0.21730, 0.21630, 0.21530, 0.21430,   0.21330, 0.21240, 0.21140, 0.21050, 0.20950, 0.20720, 0.205,   0.20270,
             0.20060, 0.19850, 0.19640, 0.19440, 0.19240, 0.19040, 0.18850,   0.18480, 0.18130, 0.17790, 0.17460, 0.17140, 0.16830, 0.16540, 0.16250,
             0.15980, 0.15710, 0.15460, 0.15210, 0.14970, 0.14730, 3.542e-05, 0.131,   0.1225},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.29370, 0.29190,   0.29020, 0.28850, 0.28690, 0.28520, 0.28360, 0.282,   0.28040, 0.27880,
             0.278,   0.27730, 0.27650, 0.27570, 0.275,   0.27420, 0.27350,   0.27270, 0.272,   0.27130, 0.27050, 0.26980, 0.26910, 0.26840, 0.26760,
             0.26690, 0.26620, 0.26550, 0.26480, 0.26410, 0.26340, 0.26280,   0.26210, 0.26140, 0.26070, 0.26,    0.25940, 0.25870, 0.258,   0.25740,
             0.25670, 0.25610, 0.25480, 0.25350, 0.25220, 0.251,   0.24980,   0.24850, 0.24730, 0.24610, 0.24490, 0.24370, 0.24260, 0.24140, 0.24030,
             0.23910, 0.238,   0.23690, 0.23580, 0.23470, 0.23360, 0.23260,   0.23150, 0.23050, 0.22940, 0.22840, 0.22740, 0.22490, 0.22240, 0.22,
             0.21770, 0.21540, 0.21310, 0.21090, 0.20880, 0.20660, 0.20460,   0.20060, 0.19670, 0.193,   0.18940, 0.186,   0.18270, 0.17950, 0.17640,
             0.17340, 0.17050, 0.16770, 0.165,   0.16240, 0.15990, 3.542e-05, 0.14210, 0.1329},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.31660,   0.31480, 0.31290, 0.31110, 0.30930, 0.30760, 0.30580, 0.30410, 0.30240,
             0.30150, 0.30070, 0.29990, 0.299,   0.29820, 0.29740, 0.29660,   0.29580, 0.295,   0.29420, 0.29340, 0.29260, 0.29180, 0.291,   0.29020,
             0.28940, 0.28870, 0.28790, 0.28720, 0.28640, 0.28560, 0.28490,   0.28420, 0.28340, 0.28270, 0.282,   0.28120, 0.28050, 0.27980, 0.27910,
             0.27840, 0.27760, 0.27620, 0.27490, 0.27350, 0.27210, 0.27080,   0.26940, 0.26810, 0.26680, 0.26550, 0.26430, 0.263,   0.26170, 0.26050,
             0.25930, 0.258,   0.25680, 0.25560, 0.25450, 0.25330, 0.25210,   0.251,   0.24980, 0.24870, 0.24760, 0.24650, 0.24380, 0.24110, 0.23850,
             0.23590, 0.23350, 0.231,   0.22860, 0.22630, 0.224,   0.22170,   0.21740, 0.21320, 0.20920, 0.20530, 0.20160, 0.198,   0.19450, 0.19120,
             0.18790, 0.18480, 0.18180, 0.17880, 0.176,   0.17330, 3.542e-05, 0.154,   0.1441},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.34110, 0.33910, 0.33710, 0.33520, 0.33320, 0.33130, 0.32940, 0.32760,
             0.32670, 0.32580, 0.32490, 0.324,   0.32310, 0.32220, 0.32130,   0.32040, 0.31950, 0.31870, 0.31780, 0.31690, 0.31610, 0.31520, 0.31440,
             0.31350, 0.31270, 0.31190, 0.31110, 0.31020, 0.30940, 0.30860,   0.30780, 0.307,   0.30620, 0.30540, 0.30460, 0.30380, 0.30310, 0.30230,
             0.30150, 0.30070, 0.29920, 0.29770, 0.29620, 0.29470, 0.29330,   0.29180, 0.29040, 0.289,   0.28760, 0.28620, 0.28480, 0.28350, 0.28210,
             0.28080, 0.27950, 0.27820, 0.27690, 0.27560, 0.27430, 0.27310,   0.27180, 0.27060, 0.26930, 0.26810, 0.26690, 0.264,   0.26110, 0.25830,
             0.25550, 0.25280, 0.25020, 0.24760, 0.245,   0.24260, 0.24010,   0.23540, 0.23090, 0.22650, 0.22230, 0.21830, 0.21440, 0.21060, 0.207,
             0.20350, 0.20010, 0.19680, 0.19360, 0.19060, 0.18760, 3.542e-05, 0.16680, 0.156},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.36710, 0.36490, 0.36280, 0.36070, 0.35860, 0.35660, 0.35460,
             0.35360, 0.35260, 0.35160, 0.35060, 0.34960, 0.34870, 0.34770,   0.34680, 0.34580, 0.34490, 0.34390, 0.343,   0.34210, 0.34110, 0.34020,
             0.33930, 0.33840, 0.33750, 0.33660, 0.33570, 0.33480, 0.334,     0.33310, 0.33220, 0.33130, 0.33050, 0.32960, 0.32880, 0.32790, 0.32710,
             0.32630, 0.32540, 0.32380, 0.32210, 0.32050, 0.31890, 0.31730,   0.31580, 0.31420, 0.31270, 0.31120, 0.30970, 0.30820, 0.30670, 0.30520,
             0.30380, 0.30240, 0.30090, 0.29950, 0.29820, 0.29680, 0.29540,   0.29410, 0.29270, 0.29140, 0.29010, 0.28880, 0.28560, 0.28250, 0.27940,
             0.27640, 0.27350, 0.27060, 0.26780, 0.26510, 0.26240, 0.25980,   0.25460, 0.24970, 0.245,   0.24050, 0.23610, 0.23190, 0.22780, 0.22390,
             0.22010, 0.21640, 0.21290, 0.20940, 0.20610, 0.20290, 3.542e-05, 0.18040, 0.1687},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.39460, 0.39230, 0.39010, 0.38780, 0.38560, 0.38340,
             0.38230, 0.38120, 0.38020, 0.37910, 0.37810, 0.377,   0.376,     0.37490, 0.37390, 0.37290, 0.37190, 0.37080, 0.36980, 0.36880, 0.36780,
             0.36690, 0.36590, 0.36490, 0.36390, 0.363,   0.362,   0.361,     0.36010, 0.35920, 0.35820, 0.35730, 0.35640, 0.35540, 0.35450, 0.35360,
             0.35270, 0.35180, 0.35,    0.34820, 0.34650, 0.34470, 0.343,     0.34130, 0.33970, 0.338,   0.33640, 0.33470, 0.33310, 0.33150, 0.32990,
             0.32840, 0.32680, 0.32530, 0.32380, 0.32230, 0.32080, 0.31930,   0.31780, 0.31640, 0.315,   0.31350, 0.31210, 0.30870, 0.30530, 0.302,
             0.29870, 0.29560, 0.29250, 0.28940, 0.28650, 0.28360, 0.28070,   0.27520, 0.26990, 0.26480, 0.25990, 0.25510, 0.25060, 0.24620, 0.24190,
             0.23780, 0.23390, 0.23,    0.22630, 0.22270, 0.21930, 3.542e-05, 0.19490, 0.1823},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.42390, 0.42140, 0.419,   0.41660, 0.41420,
             0.413,   0.41190, 0.41070, 0.40960, 0.40840, 0.40730, 0.40610,   0.405,   0.40390, 0.40280, 0.40170, 0.40060, 0.39950, 0.39840, 0.39730,
             0.39630, 0.39520, 0.39410, 0.39310, 0.392,   0.391,   0.39,      0.38890, 0.38790, 0.38690, 0.38590, 0.38490, 0.38390, 0.38290, 0.38190,
             0.38090, 0.37990, 0.378,   0.37610, 0.37420, 0.37230, 0.37050,   0.36860, 0.36680, 0.365,   0.36320, 0.36150, 0.35970, 0.358,   0.35630,
             0.35460, 0.35290, 0.35130, 0.34960, 0.348,   0.34640, 0.34480,   0.34320, 0.34160, 0.34010, 0.33860, 0.337,   0.33330, 0.32960, 0.32610,
             0.32260, 0.31910, 0.31580, 0.31250, 0.30930, 0.30620, 0.30310,   0.29710, 0.29140, 0.28590, 0.28060, 0.27540, 0.27050, 0.26580, 0.26120,
             0.25680, 0.25250, 0.24830, 0.24430, 0.24050, 0.23670, 3.542e-05, 0.21040, 0.1968},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.45490, 0.45230, 0.44970, 0.44710,
             0.44580, 0.44450, 0.44330, 0.442,   0.44080, 0.43960, 0.43830,   0.43710, 0.43590, 0.43470, 0.43350, 0.43230, 0.43110, 0.43,    0.42880,
             0.42760, 0.42650, 0.42530, 0.42420, 0.42310, 0.42190, 0.42080,   0.41970, 0.41860, 0.41750, 0.41640, 0.41530, 0.41420, 0.41320, 0.41210,
             0.411,   0.41,    0.40790, 0.40580, 0.40380, 0.40170, 0.39970,   0.39770, 0.39580, 0.39380, 0.39190, 0.39,    0.38810, 0.38620, 0.38440,
             0.38260, 0.38080, 0.379,   0.37720, 0.37540, 0.37370, 0.372,     0.37030, 0.36860, 0.36690, 0.36520, 0.36360, 0.35950, 0.35560, 0.35170,
             0.34790, 0.34420, 0.34060, 0.33710, 0.33360, 0.33020, 0.32690,   0.32050, 0.31430, 0.30830, 0.30260, 0.29710, 0.29180, 0.28660, 0.28170,
             0.27690, 0.27230, 0.26780, 0.26350, 0.25930, 0.25530, 3.542e-05, 0.22690, 0.2122},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.48780, 0.48490, 0.48210,
             0.48080, 0.47940, 0.478,   0.47670, 0.47530, 0.474,   0.47270,   0.47130, 0.47,    0.46870, 0.46740, 0.46620, 0.46490, 0.46360, 0.46230,
             0.46110, 0.45980, 0.45860, 0.45740, 0.45610, 0.45490, 0.45370,   0.45250, 0.45130, 0.45010, 0.44890, 0.44780, 0.44660, 0.44540, 0.44430,
             0.44310, 0.442,   0.43970, 0.43750, 0.43530, 0.43310, 0.43090,   0.42870, 0.42660, 0.42450, 0.42240, 0.42040, 0.41830, 0.41630, 0.41430,
             0.41240, 0.41040, 0.40850, 0.40650, 0.40460, 0.40280, 0.40090,   0.39910, 0.39720, 0.39540, 0.39360, 0.39190, 0.38750, 0.38320, 0.37910,
             0.375,   0.371,   0.36710, 0.36330, 0.35950, 0.35590, 0.35230,   0.34530, 0.33870, 0.33230, 0.32610, 0.32010, 0.31440, 0.30890, 0.30350,
             0.29840, 0.29340, 0.28860, 0.28390, 0.27940, 0.27510, 3.542e-05, 0.24450, 0.2287},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.52250, 0.51950,
             0.518,   0.51650, 0.51510, 0.51360, 0.51210, 0.51070, 0.50920,   0.50780, 0.50640, 0.505,   0.50360, 0.50220, 0.50080, 0.49940, 0.49810,
             0.49670, 0.49540, 0.494,   0.49270, 0.49140, 0.49010, 0.48870,   0.48740, 0.48610, 0.48490, 0.48360, 0.48230, 0.481,   0.47980, 0.47850,
             0.47730, 0.47610, 0.47360, 0.47120, 0.46880, 0.46640, 0.46410,   0.46180, 0.45950, 0.45720, 0.455,   0.45270, 0.45050, 0.44840, 0.44620,
             0.44410, 0.442,   0.43990, 0.43780, 0.43580, 0.43370, 0.43170,   0.42970, 0.42780, 0.42580, 0.42390, 0.422,   0.41730, 0.41270, 0.40820,
             0.40380, 0.39950, 0.39530, 0.39110, 0.38710, 0.38320, 0.37930,   0.37180, 0.36460, 0.35770, 0.35110, 0.34460, 0.33850, 0.33250, 0.32680,
             0.32120, 0.31590, 0.31070, 0.30570, 0.30080, 0.29610, 3.542e-05, 0.26320, 0.2461},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.55930,
             0.55770, 0.55610, 0.55450, 0.55290, 0.55130, 0.54980, 0.54820,   0.54670, 0.54510, 0.54360, 0.54210, 0.54060, 0.53910, 0.53760, 0.53610,
             0.53460, 0.53320, 0.53170, 0.53030, 0.52890, 0.52740, 0.526,     0.52460, 0.52320, 0.52180, 0.52050, 0.51910, 0.51770, 0.51640, 0.515,
             0.51370, 0.51230, 0.50970, 0.50710, 0.50450, 0.50190, 0.49940,   0.49690, 0.49440, 0.492,   0.48960, 0.48720, 0.48480, 0.48240, 0.48010,
             0.47780, 0.47550, 0.47330, 0.47110, 0.46880, 0.46670, 0.46450,   0.46230, 0.46020, 0.45810, 0.456,   0.454,   0.44890, 0.44390, 0.43910,
             0.43440, 0.42970, 0.42520, 0.42080, 0.41640, 0.41220, 0.408,     0.4,     0.39220, 0.38480, 0.37760, 0.37070, 0.36410, 0.35760, 0.35150,
             0.34550, 0.33970, 0.33410, 0.32870, 0.32350, 0.31850, 3.542e-05, 0.28310, 0.2647},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.57850, 0.57680, 0.57510, 0.57350, 0.57180, 0.57020, 0.56860,   0.567,   0.56540, 0.56380, 0.56220, 0.56070, 0.55910, 0.55760, 0.556,
             0.55450, 0.553,   0.55150, 0.55,    0.54850, 0.547,   0.54550,   0.54410, 0.54260, 0.54120, 0.53980, 0.53830, 0.53690, 0.53550, 0.53410,
             0.53270, 0.53130, 0.52860, 0.52590, 0.52320, 0.52050, 0.51790,   0.51530, 0.51270, 0.51020, 0.50770, 0.50520, 0.50270, 0.50030, 0.49790,
             0.49550, 0.49310, 0.49080, 0.48850, 0.48620, 0.48390, 0.48160,   0.47940, 0.47720, 0.475,   0.47290, 0.47070, 0.46550, 0.46030, 0.45530,
             0.45040, 0.44560, 0.44090, 0.43630, 0.43180, 0.42740, 0.423,     0.41470, 0.40660, 0.39890, 0.39150, 0.38430, 0.37740, 0.37080, 0.36440,
             0.35820, 0.35220, 0.34640, 0.34080, 0.33540, 0.33020, 3.542e-05, 0.29350, 0.2744},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.59820, 0.59640, 0.59470, 0.593,   0.59130, 0.58960,   0.588,   0.58630, 0.58470, 0.583,   0.58140, 0.57980, 0.57820, 0.57660,
             0.575,   0.57340, 0.57180, 0.57030, 0.56870, 0.56720, 0.56570,   0.56420, 0.56270, 0.56120, 0.55970, 0.55820, 0.55670, 0.55520, 0.55380,
             0.55230, 0.55090, 0.548,   0.54520, 0.54240, 0.53970, 0.53690,   0.53420, 0.53160, 0.52890, 0.52630, 0.52370, 0.52120, 0.51870, 0.51620,
             0.51370, 0.51120, 0.50880, 0.50640, 0.504,   0.50170, 0.49930,   0.497,   0.49470, 0.49250, 0.49020, 0.488,   0.48250, 0.47720, 0.472,
             0.46690, 0.46190, 0.457,   0.45220, 0.44760, 0.443,   0.43850,   0.42980, 0.42150, 0.41350, 0.40580, 0.39840, 0.39120, 0.38430, 0.37770,
             0.37130, 0.36510, 0.35910, 0.35330, 0.34760, 0.34220, 3.542e-05, 0.30420, 0.2844},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.61840, 0.61660, 0.61480, 0.61310, 0.61130,   0.60960, 0.60790, 0.60620, 0.60450, 0.60280, 0.60110, 0.59940, 0.59780,
             0.59610, 0.59450, 0.59280, 0.59120, 0.58960, 0.588,   0.58640,   0.58490, 0.58330, 0.58170, 0.58020, 0.57860, 0.57710, 0.57560, 0.57410,
             0.57260, 0.57110, 0.56810, 0.56520, 0.56230, 0.55940, 0.55660,   0.55380, 0.551,   0.54830, 0.54560, 0.54290, 0.54020, 0.53760, 0.535,
             0.53240, 0.52990, 0.52740, 0.52490, 0.52240, 0.52,    0.51750,   0.51510, 0.51280, 0.51040, 0.50810, 0.50580, 0.50010, 0.49460, 0.48920,
             0.48390, 0.47870, 0.47360, 0.46870, 0.46390, 0.45910, 0.45450,   0.44550, 0.43680, 0.42850, 0.42050, 0.41290, 0.40540, 0.39830, 0.39140,
             0.38470, 0.37830, 0.37210, 0.36610, 0.36030, 0.35460, 3.542e-05, 0.31520, 0.2948},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.63920, 0.63740, 0.63550, 0.63370,   0.63190, 0.63010, 0.62830, 0.62660, 0.62480, 0.623,   0.62130, 0.61960,
             0.61790, 0.61620, 0.61450, 0.61280, 0.61110, 0.60950, 0.60780,   0.60620, 0.60460, 0.60290, 0.60130, 0.59970, 0.59810, 0.59660, 0.595,
             0.59340, 0.59190, 0.58880, 0.58580, 0.58270, 0.57980, 0.57680,   0.57390, 0.571,   0.56820, 0.56540, 0.56260, 0.55990, 0.55710, 0.55440,
             0.55180, 0.54910, 0.54650, 0.54390, 0.54140, 0.53880, 0.53630,   0.53380, 0.53140, 0.52890, 0.52650, 0.52410, 0.51820, 0.51250, 0.50690,
             0.50140, 0.496,   0.49080, 0.48570, 0.48060, 0.47570, 0.47090,   0.46160, 0.45260, 0.444,   0.43570, 0.42780, 0.42010, 0.41270, 0.40550,
             0.39860, 0.392,   0.38550, 0.37930, 0.37330, 0.36740, 3.542e-05, 0.32660, 0.3054},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.66060, 0.65870, 0.65680,   0.65490, 0.653,   0.65120, 0.64930, 0.64750, 0.64570, 0.64390, 0.64210,
             0.64030, 0.63850, 0.63680, 0.635,   0.63330, 0.63160, 0.62990,   0.62820, 0.62650, 0.62480, 0.62310, 0.62150, 0.61980, 0.61820, 0.61650,
             0.61490, 0.61330, 0.61010, 0.607,   0.60380, 0.60070, 0.59770,   0.59470, 0.59170, 0.58870, 0.58580, 0.58290, 0.58010, 0.57720, 0.57440,
             0.57170, 0.56890, 0.56620, 0.56350, 0.56090, 0.55820, 0.55560,   0.55310, 0.55050, 0.548,   0.54550, 0.543,   0.53690, 0.53090, 0.52510,
             0.51940, 0.51390, 0.50840, 0.50310, 0.49790, 0.49280, 0.48780,   0.47820, 0.46890, 0.46,    0.45140, 0.44310, 0.43510, 0.42750, 0.42010,
             0.41290, 0.406,   0.39930, 0.39290, 0.38660, 0.38060, 3.542e-05, 0.33830, 0.3163},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.68250, 0.68050,   0.67860, 0.67660, 0.67470, 0.67280, 0.67090, 0.669,   0.66710, 0.66530,
             0.66340, 0.66160, 0.65980, 0.658,   0.65620, 0.65440, 0.65260,   0.65080, 0.64910, 0.64730, 0.64560, 0.64390, 0.64210, 0.64040, 0.63870,
             0.63710, 0.63540, 0.63210, 0.62880, 0.62550, 0.62230, 0.61920,   0.616,   0.61290, 0.60990, 0.60690, 0.60390, 0.60090, 0.598,   0.59510,
             0.59220, 0.58930, 0.58650, 0.58370, 0.581,   0.57830, 0.57560,   0.57290, 0.57020, 0.56760, 0.565,   0.56240, 0.55610, 0.54990, 0.54390,
             0.538,   0.53230, 0.52660, 0.52110, 0.51570, 0.51040, 0.50530,   0.49520, 0.48560, 0.47640, 0.46750, 0.45890, 0.45070, 0.44270, 0.435,
             0.42760, 0.42050, 0.41360, 0.40690, 0.40040, 0.39410, 3.542e-05, 0.35030, 0.3276},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.705,     0.703,   0.701,   0.699,   0.697,   0.695,   0.69310, 0.69110, 0.68920,
             0.68730, 0.68530, 0.68350, 0.68160, 0.67970, 0.67780, 0.676,     0.67420, 0.67230, 0.67050, 0.66870, 0.66690, 0.66510, 0.66340, 0.66160,
             0.65990, 0.65810, 0.65470, 0.65130, 0.64790, 0.64460, 0.64130,   0.63810, 0.63480, 0.63170, 0.62850, 0.62540, 0.62230, 0.61930, 0.61630,
             0.61330, 0.61040, 0.60740, 0.60460, 0.60170, 0.59890, 0.59610,   0.59330, 0.59050, 0.58780, 0.58510, 0.58250, 0.57590, 0.56950, 0.56330,
             0.55710, 0.55120, 0.54530, 0.53960, 0.534,   0.52860, 0.52320,   0.51280, 0.50280, 0.49330, 0.484,   0.47520, 0.46660, 0.45840, 0.45050,
             0.44280, 0.43540, 0.42820, 0.42130, 0.41460, 0.40810, 3.542e-05, 0.36270, 0.3391},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.72820, 0.72610, 0.724,   0.72190, 0.71990, 0.71780, 0.71580, 0.71380,
             0.71180, 0.70980, 0.70790, 0.70590, 0.704,   0.702,   0.70010,   0.69820, 0.69630, 0.69440, 0.69250, 0.69070, 0.68880, 0.687,   0.68520,
             0.68340, 0.68160, 0.678,   0.67450, 0.671,   0.66750, 0.66410,   0.66070, 0.65740, 0.65410, 0.65080, 0.64760, 0.64440, 0.64130, 0.63810,
             0.63510, 0.632,   0.629,   0.626,   0.623,   0.62010, 0.61720,   0.61430, 0.61150, 0.60860, 0.60580, 0.60310, 0.59630, 0.58960, 0.58320,
             0.57680, 0.57060, 0.56460, 0.55870, 0.55290, 0.54720, 0.54170,   0.53090, 0.52060, 0.51060, 0.50110, 0.49190, 0.48310, 0.47450, 0.46630,
             0.45840, 0.45070, 0.44330, 0.43610, 0.42920, 0.42240, 3.542e-05, 0.37540, 0.3511},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.75190, 0.74970, 0.74760, 0.74550, 0.74330, 0.74120, 0.73920,
             0.73710, 0.735,   0.733,   0.73090, 0.72890, 0.72690, 0.72490,   0.723,   0.721,   0.719,   0.71710, 0.71520, 0.71320, 0.71130, 0.70940,
             0.70760, 0.70570, 0.702,   0.69830, 0.69470, 0.69110, 0.68760,   0.68410, 0.68060, 0.67720, 0.67380, 0.67050, 0.66720, 0.66390, 0.66060,
             0.65740, 0.65430, 0.65110, 0.648,   0.645,   0.64190, 0.63890,   0.63590, 0.633,   0.63010, 0.62720, 0.62430, 0.61730, 0.61040, 0.60370,
             0.59710, 0.59070, 0.58440, 0.57830, 0.57230, 0.56640, 0.56070,   0.54950, 0.53880, 0.52850, 0.51870, 0.50910, 0.5,     0.49120, 0.48260,
             0.47440, 0.46650, 0.45880, 0.45140, 0.44420, 0.43720, 3.542e-05, 0.38860, 0.3633},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.77630, 0.774,   0.77180, 0.76960, 0.76740, 0.76530,
             0.76310, 0.761,   0.75890, 0.75670, 0.75470, 0.75260, 0.75050,   0.74840, 0.74640, 0.74440, 0.74240, 0.74040, 0.73840, 0.73640, 0.73440,
             0.73250, 0.73050, 0.72670, 0.72290, 0.71910, 0.71540, 0.71170,   0.70810, 0.70450, 0.701,   0.69750, 0.694,   0.69060, 0.68720, 0.68380,
             0.68050, 0.67720, 0.674,   0.67070, 0.66760, 0.66440, 0.66130,   0.65820, 0.65510, 0.65210, 0.64910, 0.64610, 0.63880, 0.63170, 0.62480,
             0.618,   0.61130, 0.60480, 0.59850, 0.59230, 0.58620, 0.58020,   0.56870, 0.55760, 0.547,   0.53670, 0.52690, 0.51740, 0.50820, 0.49940,
             0.49090, 0.48270, 0.47470, 0.46710, 0.45960, 0.45240, 3.542e-05, 0.40210, 0.3759},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.80130, 0.799,   0.79670, 0.79440, 0.79220,
             0.78990, 0.78770, 0.78550, 0.78330, 0.78110, 0.779,   0.77680,   0.77470, 0.77260, 0.77050, 0.76840, 0.76630, 0.76420, 0.76220, 0.76010,
             0.75810, 0.75610, 0.75210, 0.74820, 0.74430, 0.74040, 0.73660,   0.73280, 0.72910, 0.72540, 0.72180, 0.71820, 0.71470, 0.71110, 0.70770,
             0.70420, 0.70080, 0.69740, 0.69410, 0.69080, 0.68750, 0.68430,   0.68110, 0.67790, 0.67480, 0.67170, 0.66860, 0.661,   0.65370, 0.64650,
             0.63940, 0.63250, 0.62580, 0.61920, 0.61280, 0.60650, 0.60030,   0.58840, 0.57690, 0.56590, 0.55530, 0.54510, 0.53530, 0.52580, 0.51670,
             0.50790, 0.49940, 0.49110, 0.48320, 0.47550, 0.468,   3.542e-05, 0.41590, 0.3889},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.82690, 0.82460, 0.82220, 0.81990,
             0.81750, 0.81520, 0.81290, 0.81070, 0.80840, 0.80620, 0.80390,   0.80170, 0.79950, 0.79730, 0.79520, 0.793,   0.79090, 0.78870, 0.78660,
             0.78450, 0.78240, 0.77830, 0.77420, 0.77010, 0.76610, 0.76220,   0.75830, 0.75440, 0.75060, 0.74690, 0.74310, 0.73940, 0.73580, 0.73220,
             0.72860, 0.72510, 0.72160, 0.71810, 0.71470, 0.71130, 0.708,     0.70470, 0.70140, 0.69810, 0.69490, 0.69170, 0.68390, 0.67630, 0.66880,
             0.66150, 0.65440, 0.64740, 0.64060, 0.63390, 0.62740, 0.621,     0.60870, 0.59680, 0.58540, 0.57440, 0.56390, 0.55370, 0.54390, 0.53450,
             0.52530, 0.51650, 0.508,   0.49980, 0.49180, 0.48410, 3.542e-05, 0.43020, 0.4023},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.85320, 0.85080, 0.84840,
             0.846,   0.84360, 0.84120, 0.83880, 0.83650, 0.83410, 0.83180,   0.82950, 0.82730, 0.825,   0.82270, 0.82050, 0.81830, 0.81610, 0.81390,
             0.81170, 0.80950, 0.80520, 0.801,   0.79680, 0.79260, 0.78850,   0.78450, 0.78050, 0.77650, 0.77260, 0.76880, 0.76490, 0.76120, 0.75740,
             0.75370, 0.75010, 0.74650, 0.74290, 0.73930, 0.73580, 0.73240,   0.72890, 0.72550, 0.72210, 0.71880, 0.71550, 0.70740, 0.69950, 0.69180,
             0.68420, 0.67680, 0.66960, 0.66260, 0.65570, 0.64890, 0.64230,   0.62950, 0.61720, 0.60540, 0.59410, 0.58310, 0.57260, 0.56250, 0.55270,
             0.54330, 0.53420, 0.52540, 0.51690, 0.50860, 0.50060, 3.542e-05, 0.44490, 0.416},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.88020, 0.87770,
             0.87520, 0.87270, 0.87030, 0.86780, 0.86540, 0.86290, 0.86050,   0.85820, 0.85580, 0.85340, 0.85110, 0.84880, 0.84650, 0.84420, 0.84190,
             0.83960, 0.83740, 0.83290, 0.82850, 0.82420, 0.81990, 0.81560,   0.81140, 0.80730, 0.80320, 0.79920, 0.79510, 0.79120, 0.78730, 0.78340,
             0.77960, 0.77580, 0.772,   0.76830, 0.76460, 0.761,   0.75740,   0.75390, 0.75030, 0.74680, 0.74340, 0.74,    0.73160, 0.72340, 0.71540,
             0.70760, 0.69990, 0.69240, 0.68510, 0.678,   0.671,   0.66420,   0.65090, 0.63820, 0.626,   0.61430, 0.603,   0.59210, 0.58160, 0.57150,
             0.56170, 0.55230, 0.54320, 0.53440, 0.52590, 0.51760, 3.542e-05, 0.46,    0.4301},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.90790,
             0.90530, 0.90270, 0.90020, 0.89760, 0.89510, 0.89260, 0.89010,   0.88760, 0.88520, 0.88270, 0.88030, 0.87790, 0.87550, 0.87310, 0.87070,
             0.86840, 0.86610, 0.86140, 0.85690, 0.85240, 0.84790, 0.84350,   0.83920, 0.83490, 0.83060, 0.82640, 0.82230, 0.81820, 0.81410, 0.81010,
             0.80610, 0.80220, 0.79830, 0.79450, 0.79070, 0.78690, 0.78320,   0.77950, 0.77590, 0.77220, 0.76870, 0.76510, 0.75640, 0.74790, 0.73970,
             0.73160, 0.72370, 0.71590, 0.70840, 0.701,   0.69380, 0.68670,   0.673,   0.65980, 0.64720, 0.635,   0.62340, 0.61210, 0.60130, 0.59080,
             0.58070, 0.571,   0.56150, 0.55240, 0.54360, 0.53510, 3.542e-05, 0.47550, 0.4446},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.93630, 0.93360, 0.931,   0.92830, 0.92570, 0.92310, 0.92050,   0.91790, 0.91540, 0.91280, 0.91030, 0.90780, 0.90530, 0.90290, 0.90040,
             0.898,   0.89560, 0.89080, 0.886,   0.88140, 0.87680, 0.87220,   0.86770, 0.86320, 0.85880, 0.85450, 0.85020, 0.84590, 0.84170, 0.83760,
             0.83340, 0.82940, 0.82540, 0.82140, 0.81740, 0.81350, 0.80970,   0.80590, 0.80210, 0.79840, 0.79460, 0.791,   0.782,   0.77320, 0.76460,
             0.75620, 0.74810, 0.74010, 0.73220, 0.72460, 0.71710, 0.70980,   0.69560, 0.682,   0.66890, 0.65640, 0.64430, 0.63270, 0.62150, 0.61060,
             0.60020, 0.59010, 0.58040, 0.571,   0.56190, 0.553,   3.542e-05, 0.49140, 0.4594},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.96540, 0.96260, 0.95990, 0.95720, 0.95450, 0.95180,   0.94910, 0.94650, 0.94380, 0.94120, 0.93860, 0.93610, 0.93350, 0.93090,
             0.92840, 0.92590, 0.92090, 0.916,   0.91120, 0.90640, 0.90170,   0.897,   0.89240, 0.88780, 0.88330, 0.87890, 0.87450, 0.87010, 0.86580,
             0.86150, 0.85730, 0.85320, 0.849,   0.845,   0.84090, 0.83690,   0.833,   0.82910, 0.82520, 0.82140, 0.81760, 0.80830, 0.79920, 0.79030,
             0.78160, 0.77310, 0.76490, 0.75680, 0.74890, 0.74110, 0.73360,   0.71890, 0.70480, 0.69130, 0.67830, 0.66580, 0.65380, 0.64220, 0.631,
             0.62020, 0.60980, 0.59970, 0.59,    0.58060, 0.57150, 3.542e-05, 0.50780, 0.4747},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.99520, 0.99240, 0.98950, 0.98670, 0.984,     0.98120, 0.97840, 0.97570, 0.973,   0.97030, 0.96760, 0.965,   0.96230,
             0.95970, 0.95710, 0.952,   0.94690, 0.94190, 0.93690, 0.932,     0.92720, 0.92240, 0.91770, 0.913,   0.90840, 0.90380, 0.89930, 0.89480,
             0.89040, 0.88610, 0.88170, 0.87750, 0.87320, 0.86910, 0.86490,   0.86080, 0.85680, 0.85280, 0.84880, 0.84490, 0.83520, 0.82580, 0.81670,
             0.80770, 0.79890, 0.79040, 0.782,   0.77380, 0.76580, 0.758,     0.74280, 0.72830, 0.71430, 0.70090, 0.68790, 0.67550, 0.66350, 0.652,
             0.64080, 0.63,    0.61960, 0.60960, 0.59980, 0.59040, 3.542e-05, 0.52460, 0.4905},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     1.026,   1.023,   1.02,    1.017,     1.014,   1.011,   1.008,   1.006,   1.003,   1.0,     0.99740, 0.99460,
             0.99190, 0.98920, 0.98390, 0.97860, 0.97340, 0.96830, 0.96320,   0.95820, 0.95320, 0.94830, 0.94350, 0.93870, 0.934,   0.92930, 0.92470,
             0.92010, 0.91560, 0.91110, 0.90670, 0.90230, 0.898,   0.89370,   0.88950, 0.88530, 0.88110, 0.877,   0.873,   0.863,   0.85330, 0.84380,
             0.83450, 0.82540, 0.81660, 0.80790, 0.79940, 0.79120, 0.78310,   0.76740, 0.75230, 0.73790, 0.724,   0.71060, 0.69780, 0.68540, 0.67350,
             0.66190, 0.65080, 0.64010, 0.62970, 0.61960, 0.60990, 3.542e-05, 0.54180, 0.5066},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     1.057,   1.054,   1.051,     1.048,   1.045,   1.042,   1.039,   1.036,   1.034,   1.031,   1.028,
             1.025,   1.022,   1.017,   1.011,   1.006,   1.0,     0.99520,   0.99,    0.98490, 0.97980, 0.97480, 0.96990, 0.965,   0.96010, 0.95530,
             0.95060, 0.94590, 0.94130, 0.93670, 0.93220, 0.92770, 0.92330,   0.91890, 0.91460, 0.91030, 0.906,   0.90180, 0.89150, 0.88140, 0.87160,
             0.862,   0.85260, 0.84350, 0.83450, 0.82580, 0.81720, 0.80880,   0.79260, 0.77710, 0.76210, 0.74780, 0.734,   0.72070, 0.70790, 0.69550,
             0.68360, 0.67210, 0.661,   0.65030, 0.63990, 0.62980, 3.542e-05, 0.55960, 0.5232},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     1.089,   1.086,     1.083,   1.08,    1.077,   1.074,   1.071,   1.068,   1.065,   1.062,
             1.059,   1.056,   1.05,    1.045,   1.039,   1.034,   1.028,     1.023,   1.017,   1.012,   1.007,   1.002,   0.99680, 0.99180, 0.98680,
             0.982,   0.97710, 0.97230, 0.96760, 0.96290, 0.95830, 0.95370,   0.94910, 0.94470, 0.94020, 0.93580, 0.93150, 0.92080, 0.91040, 0.90020,
             0.89030, 0.88060, 0.87110, 0.86190, 0.85280, 0.844,   0.83530,   0.81850, 0.80250, 0.787,   0.77220, 0.75790, 0.74420, 0.731,   0.71820,
             0.70590, 0.694,   0.68260, 0.67150, 0.66070, 0.65030, 3.542e-05, 0.57780, 0.5402},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     1.122,     1.119,   1.116,   1.113,   1.109,   1.106,   1.103,   1.1,     1.097,
             1.094,   1.091,   1.085,   1.079,   1.073,   1.068,   1.062,     1.056,   1.051,   1.045,   1.04,    1.035,   1.03,    1.024,   1.019,
             1.014,   1.009,   1.004,   0.99930, 0.99440, 0.98960, 0.98490,   0.98020, 0.97560, 0.971,   0.96640, 0.96190, 0.95090, 0.94010, 0.92960,
             0.91930, 0.90930, 0.89950, 0.88990, 0.88060, 0.87140, 0.86250,   0.84510, 0.82850, 0.81260, 0.79730, 0.78250, 0.76830, 0.75470, 0.74150,
             0.72880, 0.71650, 0.70470, 0.69320, 0.68210, 0.67140, 3.542e-05, 0.59640, 0.5576},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       1.156,   1.152,   1.149,   1.146,   1.143,   1.139,   1.136,   1.133,
             1.13,    1.127,   1.121,   1.115,   1.109,   1.103,   1.097,     1.091,   1.085,   1.08,    1.074,   1.069,   1.063,   1.058,   1.052,
             1.047,   1.042,   1.037,   1.032,   1.027,   1.022,   1.017,     1.012,   1.007,   1.003,   0.99790, 0.99320, 0.98180, 0.97060, 0.95970,
             0.94910, 0.93880, 0.92860, 0.91880, 0.90910, 0.89960, 0.89040,   0.87250, 0.85530, 0.83880, 0.823,   0.80780, 0.79310, 0.779,   0.76540,
             0.75230, 0.73960, 0.72740, 0.71550, 0.70410, 0.693,   3.542e-05, 0.61560, 0.5755},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,   0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,   0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,   0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     1.19,    1.187,   1.183,   1.18,    1.177,   1.173, 1.17,
             1.167,   1.164,   1.157,   1.151,   1.145,   1.139,   1.133,     1.127,   1.121,   1.115,   1.109,   1.103,   1.098,   1.092, 1.087,
             1.081,   1.076,   1.071,   1.065,   1.06,    1.055,   1.05,      1.045,   1.04,    1.035,   1.03,    1.025,   1.013,   1.002, 0.99070,
             0.97970, 0.969,   0.95860, 0.94840, 0.93840, 0.92860, 0.919,     0.90050, 0.88280, 0.86580, 0.84940, 0.83370, 0.81860, 0.804, 0.78990,
             0.77640, 0.76330, 0.75070, 0.73840, 0.72660, 0.71520, 3.542e-05, 0.63530, 0.5939},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,   1.226,   1.222,   1.219,   1.215,   1.212,   1.208,
             1.205,   1.202,   1.195,   1.188,   1.182,   1.176,   1.169,     1.163,   1.157, 1.151,   1.145,   1.139,   1.133,   1.127,   1.122,
             1.116,   1.111,   1.105,   1.1,     1.094,   1.089,   1.084,     1.079,   1.073, 1.068,   1.063,   1.058,   1.046,   1.034,   1.023,
             1.011,   1.0,     0.98930, 0.97870, 0.96840, 0.95830, 0.94840,   0.92930, 0.911, 0.89340, 0.87650, 0.86030, 0.84470, 0.82960, 0.81510,
             0.80110, 0.78760, 0.77460, 0.76190, 0.74970, 0.73790, 3.542e-05, 0.65550, 0.6128},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     1.262,   1.258,   1.254,   1.251,   1.247,
             1.244,   1.24,    1.234,   1.227,   1.22,    1.213,   1.207,     1.201,   1.194,   1.188,   1.182,   1.176,   1.17,    1.164,   1.158,
             1.152,   1.146,   1.141,   1.135,   1.129,   1.124,   1.118,     1.113,   1.108,   1.102,   1.097,   1.092,   1.08,    1.067,   1.055,
             1.043,   1.032,   1.021,   1.01,    0.99920, 0.98880, 0.97860,   0.95890, 0.93990, 0.92180, 0.90440, 0.88760, 0.87150, 0.85590, 0.84090,
             0.82650, 0.81260, 0.79910, 0.78610, 0.77350, 0.76130, 3.542e-05, 0.67620, 0.6321},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,     0.0,     0.0,     0.0,     1.299,   1.295,   1.291,   1.288,
             1.284,   1.28,    1.273,   1.266,   1.259,   1.252,   1.246,     1.239,   1.232,   1.226,   1.22,    1.213,   1.207,   1.201,   1.195,
             1.189,   1.183,   1.177,   1.171,   1.165,   1.16,    1.154,     1.149,   1.143,   1.138,   1.132,   1.127,   1.114,   1.101,   1.089,
             1.077,   1.065,   1.053,   1.042,   1.031,   1.02,    1.01,      0.98920, 0.96960, 0.95090, 0.93290, 0.91560, 0.89890, 0.88290, 0.86740,
             0.85250, 0.83810, 0.82420, 0.81080, 0.79780, 0.78520, 3.542e-05, 0.69740, 0.652},
            {0.0,     0.0,     0.0,   0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,   0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,   0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,   0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,     0.0,     0.0,     1.337,   1.333,   1.329,
             1.325,   1.321,   1.314, 1.307,   1.3,     1.292,   1.285,     1.279,  1.272, 1.265,   1.258,   1.252,   1.245,   1.239,   1.233,
             1.227,   1.22,    1.214, 1.208,   1.202,   1.196,   1.191,     1.185,  1.179, 1.174,   1.168,   1.163,   1.149,   1.136,   1.123,
             1.111,   1.098,   1.086, 1.075,   1.063,   1.052,   1.041,     1.02,   1.0,   0.98080, 0.96220, 0.94430, 0.92710, 0.91060, 0.89460,
             0.87920, 0.86440, 0.85,  0.83620, 0.82280, 0.80980, 3.542e-05, 0.7192, 0.6723},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,   0.0,       0.0,    0.0,   0.0,   0.0,     0.0,     0.0,   0.0,   0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,   0.0,       0.0,    0.0,   0.0,   0.0,     0.0,     0.0,   0.0,   0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,   0.0,       0.0,    0.0,   0.0,   0.0,     0.0,     0.0,   0.0,   0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,   0.0,       0.0,    0.0,   0.0,   0.0,     0.0,     0.0,   1.375, 1.371,
             1.367,   1.364,   1.356,   1.348,   1.341,   1.334, 1.326,     1.319,  1.312, 1.305, 1.298,   1.292,   1.285, 1.278, 1.272,
             1.265,   1.259,   1.253,   1.246,   1.24,    1.234, 1.228,     1.222,  1.216, 1.211, 1.205,   1.199,   1.185, 1.172, 1.158,
             1.145,   1.133,   1.12,    1.108,   1.097,   1.085, 1.074,     1.052,  1.031, 1.011, 0.99220, 0.97380, 0.956, 0.939, 0.92250,
             0.90660, 0.89130, 0.87650, 0.86220, 0.84840, 0.835, 3.542e-05, 0.7416, 0.6932},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     1.415,
             1.411,   1.407,   1.399,   1.391,   1.383,   1.376,   1.368,     1.361,  1.354, 1.346, 1.339, 1.332, 1.325,   1.319,   1.312,
             1.305,   1.299,   1.292,   1.286,   1.279,   1.273,   1.267,     1.261,  1.255, 1.249, 1.243, 1.237, 1.222,   1.208,   1.195,
             1.181,   1.168,   1.155,   1.143,   1.131,   1.119,   1.107,     1.085,  1.063, 1.043, 1.023, 1.004, 0.98570, 0.96810, 0.95110,
             0.93470, 0.91890, 0.90360, 0.88890, 0.87460, 0.86080, 3.542e-05, 0.7645, 0.7146},
            {0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,     0.0,
             0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,     0.0,
             1.455,   1.451,   1.443,   1.435,   1.427,   1.419,   1.411,     1.404,  1.396, 1.389, 1.381, 1.374, 1.367, 1.36,    1.353,
             1.346,   1.339,   1.332,   1.326,   1.319,   1.313,   1.306,     1.3,    1.294, 1.287, 1.281, 1.275, 1.26,  1.246,   1.232,
             1.218,   1.204,   1.191,   1.178,   1.166,   1.154,   1.142,     1.118,  1.096, 1.075, 1.055, 1.035, 1.016, 0.99790, 0.98040,
             0.96350, 0.94720, 0.93140, 0.91620, 0.90150, 0.88730, 3.542e-05, 0.7879, 0.7365},
            {0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,     0.0,     0.0,     0.0,       1.497, 1.488, 1.48,  1.472, 1.464, 1.456, 1.448,
             1.44,  1.432, 1.425, 1.417, 1.41,    1.402,   1.395,   1.388,   1.381,   1.374,     1.367, 1.36,  1.354, 1.347, 1.34,  1.334, 1.327,
             1.321, 1.315, 1.299, 1.284, 1.27,    1.255,   1.242,   1.228,   1.215,   1.202,     1.189, 1.177, 1.153, 1.13,  1.108, 1.087, 1.067,
             1.047, 1.028, 1.01,  0.993, 0.97620, 0.95990, 0.94420, 0.92910, 0.91440, 3.542e-05, 0.812, 0.759},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,     0.0,     0.0,       0.0,    1.583, 1.574, 1.565, 1.556, 1.548, 1.539,
             1.531, 1.522, 1.514, 1.506, 1.498, 1.49,  1.483, 1.475,   1.468,   1.46,      1.453,  1.445, 1.438, 1.431, 1.424, 1.417, 1.41,
             1.404, 1.397, 1.38,  1.364, 1.349, 1.334, 1.319, 1.304,   1.29,    1.276,     1.263,  1.25,  1.224, 1.2,   1.177, 1.154, 1.133,
             1.112, 1.092, 1.073, 1.054, 1.036, 1.019, 1.002, 0.98630, 0.97070, 3.542e-05, 0.8619, 0.8056},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,    0.0,   1.673, 1.663, 1.654, 1.644, 1.635,
             1.626, 1.617, 1.609, 1.6,   1.592, 1.583, 1.575, 1.567, 1.559, 1.551,     1.543,  1.535, 1.527, 1.52,  1.512, 1.505, 1.498,
             1.49,  1.483, 1.466, 1.449, 1.432, 1.416, 1.4,   1.385, 1.37,  1.355,     1.341,  1.327, 1.299, 1.273, 1.249, 1.225, 1.202,
             1.18,  1.159, 1.138, 1.119, 1.1,   1.081, 1.063, 1.046, 1.03,  3.542e-05, 0.9143, 0.8546},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,    0.0,   0.0,   1.766, 1.756, 1.746, 1.737,
             1.727, 1.717, 1.708, 1.699, 1.69,  1.681, 1.672, 1.663, 1.655, 1.646,     1.638,  1.629, 1.621, 1.613, 1.605, 1.597, 1.589,
             1.582, 1.574, 1.555, 1.537, 1.519, 1.502, 1.485, 1.469, 1.453, 1.437,     1.422,  1.407, 1.378, 1.351, 1.324, 1.299, 1.274,
             1.251, 1.229, 1.207, 1.186, 1.166, 1.146, 1.128, 1.109, 1.092, 3.542e-05, 0.9692, 0.9059},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,   0.0,   0.0,   1.864, 1.854, 1.843,
             1.833, 1.823, 1.813, 1.803, 1.793, 1.784, 1.774, 1.765, 1.755, 1.746,     1.737, 1.729, 1.72,  1.711, 1.703, 1.694, 1.686,
             1.678, 1.669, 1.649, 1.63,  1.611, 1.593, 1.575, 1.557, 1.54,  1.524,     1.507, 1.492, 1.461, 1.432, 1.403, 1.377, 1.351,
             1.326, 1.302, 1.279, 1.257, 1.235, 1.215, 1.195, 1.175, 1.157, 3.542e-05, 1.027, 0.9597},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   1.967, 1.955, 1.944, 1.933, 1.923, 1.912, 1.902, 1.891,     1.881, 1.871,
             1.861, 1.852, 1.842, 1.833, 1.823, 1.814, 1.805, 1.796, 1.787, 1.778, 1.77,  1.748, 1.728, 1.707, 1.688, 1.669, 1.65,      1.632, 1.614,
             1.597, 1.58,  1.548, 1.516, 1.487, 1.458, 1.431, 1.404, 1.379, 1.354, 1.331, 1.308, 1.286, 1.265, 1.245, 1.225, 3.542e-05, 1.087, 1.016},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   2.074, 2.062, 2.05,  2.038, 2.027, 2.016, 2.005,     1.994, 1.983,
             1.973, 1.962, 1.952, 1.942, 1.932, 1.922, 1.912, 1.903, 1.893, 1.884, 1.875, 1.852, 1.83,  1.809, 1.788, 1.767, 1.748,     1.728, 1.709,
             1.691, 1.673, 1.639, 1.605, 1.574, 1.543, 1.514, 1.486, 1.459, 1.434, 1.409, 1.384, 1.361, 1.339, 1.317, 1.296, 3.542e-05, 1.15,  1.075},
            {0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   2.185, 2.172, 2.16,  2.148, 2.136, 2.124,     2.112, 2.101,
             2.09, 2.079, 2.068, 2.057, 2.046, 2.036, 2.025, 2.015, 2.005, 1.995, 1.985, 1.961, 1.937, 1.915, 1.892, 1.871, 1.85,      1.829, 1.809,
             1.79, 1.771, 1.734, 1.699, 1.665, 1.633, 1.602, 1.572, 1.544, 1.516, 1.49,  1.464, 1.44,  1.416, 1.393, 1.371, 3.542e-05, 1.216, 1.137},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   2.301, 2.288, 2.275, 2.262, 2.249,     2.237, 2.225,
             2.213, 2.201, 2.189, 2.177, 2.166, 2.155, 2.144, 2.133, 2.122, 2.111, 2.101, 2.075, 2.05,  2.026, 2.002, 1.979, 1.957,     1.935, 1.914,
             1.893, 1.873, 1.834, 1.796, 1.761, 1.727, 1.694, 1.662, 1.632, 1.603, 1.575, 1.548, 1.522, 1.497, 1.473, 1.449, 3.542e-05, 1.286, 1.201},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   2.422, 2.408, 2.394, 2.381,     2.367, 2.354,
             2.341, 2.329, 2.316, 2.304, 2.292, 2.28,  2.268, 2.256, 2.245, 2.233, 2.222, 2.195, 2.168, 2.142, 2.117, 2.093, 2.069,     2.046, 2.023,
             2.001, 1.98,  1.938, 1.899, 1.861, 1.825, 1.79,  1.757, 1.725, 1.694, 1.664, 1.635, 1.608, 1.581, 1.556, 1.531, 3.542e-05, 1.358, 1.269},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   2.548, 2.533, 2.519,     2.505, 2.491,
             2.477, 2.463, 2.45,  2.437, 2.424, 2.411, 2.398, 2.386, 2.373, 2.361, 2.349, 2.32,  2.292, 2.264, 2.238, 2.212, 2.186,     2.162, 2.138,
             2.114, 2.091, 2.048, 2.006, 1.965, 1.927, 1.89,  1.855, 1.821, 1.789, 1.757, 1.727, 1.698, 1.67,  1.642, 1.616, 3.542e-05, 1.433, 1.339},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   2.679, 2.664,     2.648, 2.633,
             2.619, 2.604, 2.59,  2.576, 2.562, 2.548, 2.535, 2.522, 2.508, 2.495, 2.483, 2.452, 2.421, 2.392, 2.364, 2.336, 2.309,     2.283, 2.258,
             2.233, 2.209, 2.162, 2.117, 2.075, 2.034, 1.995, 1.958, 1.922, 1.888, 1.854, 1.822, 1.792, 1.762, 1.733, 1.705, 3.542e-05, 1.512, 1.413},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   2.816,     2.8,   2.783,
             2.768, 2.752, 2.737, 2.722, 2.707, 2.692, 2.678, 2.664, 2.65,  2.636, 2.622, 2.589, 2.557, 2.526, 2.496, 2.466, 2.438,     2.41,  2.383,
             2.357, 2.331, 2.282, 2.234, 2.189, 2.146, 2.105, 2.066, 2.028, 1.991, 1.956, 1.922, 1.89,  1.858, 1.828, 1.799, 3.542e-05, 1.595, 1.490},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       2.958, 2.941,
             2.924, 2.907, 2.891, 2.875, 2.859, 2.843, 2.828, 2.813, 2.798, 2.783, 2.769, 2.733, 2.699, 2.666, 2.634, 2.603, 2.572,     2.543, 2.514,
             2.486, 2.459, 2.407, 2.357, 2.309, 2.263, 2.22,  2.178, 2.138, 2.099, 2.062, 2.026, 1.992, 1.959, 1.927, 1.896, 3.542e-05, 1.681, 1.570},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   3.106,
             3.088, 3.07,  3.052, 3.035, 3.018, 3.001, 2.985, 2.969, 2.953, 2.937, 2.922, 2.884, 2.848, 2.812, 2.778, 2.745, 2.713,     2.682, 2.651,
             2.622, 2.593, 2.537, 2.484, 2.434, 2.386, 2.34,  2.295, 2.253, 2.212, 2.173, 2.135, 2.099, 2.064, 2.03,  1.997, 3.542e-05, 1.77,  1.654},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             3.26,  3.24,  3.222, 3.203, 3.185, 3.167, 3.15,  3.132, 3.115, 3.099, 3.082, 3.042, 3.003, 2.966, 2.929, 2.894, 2.86,      2.827, 2.794,
             2.763, 2.732, 2.674, 2.618, 2.564, 2.513, 2.465, 2.418, 2.373, 2.33,  2.289, 2.249, 2.21,  2.173, 2.138, 2.103, 3.542e-05, 1.864, 1.741},
            {0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   3.419, 3.399, 3.379, 3.36, 3.341, 3.322, 3.304, 3.286, 3.268, 3.25,  3.207, 3.166, 3.126, 3.087, 3.05,  3.014,     2.978, 2.944,
             2.911, 2.878, 2.816, 2.757, 2.7,  2.646, 2.595, 2.546, 2.498, 2.453, 2.409, 2.367, 2.326, 2.287, 2.25,  2.213, 3.542e-05, 1.961, 1.832},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   3.585, 3.564, 3.543, 3.523, 3.503, 3.483, 3.464, 3.445, 3.426, 3.38, 3.336, 3.294, 3.253, 3.213, 3.174,     3.137, 3.1,
             3.065, 3.031, 2.965, 2.902, 2.842, 2.785, 2.731, 2.679, 2.629, 2.581, 2.535, 2.49, 2.448, 2.406, 2.367, 2.328, 3.542e-05, 2.063, 1.926},
            {0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,  0.0,  3.758, 3.735, 3.713, 3.692, 3.671, 3.65,  3.63,  3.61,  3.561, 3.514, 3.469, 3.425, 3.383, 3.342,     3.302, 3.264,
             3.226, 3.19, 3.12, 3.054, 2.99,  2.93,  2.873, 2.818, 2.765, 2.714, 2.665, 2.619, 2.574, 2.53,  2.488, 2.448, 3.542e-05, 2.168, 2.025},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   3.937, 3.913, 3.89, 3.867, 3.845, 3.823, 3.802, 3.75,  3.7,   3.652, 3.605, 3.561, 3.517,     3.475, 3.434,
             3.394, 3.356, 3.282, 3.212, 3.145, 3.081, 3.02, 2.962, 2.907, 2.853, 2.802, 2.752, 2.705, 2.659, 2.615, 2.573, 3.542e-05, 2.278, 2.127},
            {0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   4.122, 4.097, 4.073, 4.049, 4.026, 4.003, 3.948, 3.895, 3.843, 3.794, 3.746, 3.7,       3.655, 3.612,
             3.57, 3.529, 3.451, 3.376, 3.306, 3.238, 3.174, 3.113, 3.054, 2.998, 2.944, 2.892, 2.842, 2.794, 2.747, 2.702, 3.542e-05, 2.392, 2.234},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   4.315, 4.289, 4.263, 4.238, 4.214, 4.155, 4.098, 4.043, 3.991, 3.94,  3.891,     3.843, 3.797,
             3.753, 3.709, 3.627, 3.548, 3.473, 3.402, 3.335, 3.27,  3.208, 3.148, 3.091, 3.037, 2.984, 2.933, 2.884, 2.837, 3.542e-05, 2.511, 2.344},
            {0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   4.515, 4.487, 4.46,  4.434, 4.371, 4.31,  4.252, 4.196, 4.142, 4.09,      4.04,  3.991,
             3.944, 3.898, 3.81, 3.727, 3.648, 3.573, 3.501, 3.433, 3.368, 3.305, 3.245, 3.187, 3.132, 3.079, 3.027, 2.977, 3.542e-05, 2.635, 2.459},
            {0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   4.722, 4.693, 4.665, 4.597, 4.532, 4.47, 4.411, 4.353, 4.298,     4.244, 4.193,
             4.143, 4.094, 4.001, 3.913, 3.83, 3.751, 3.675, 3.603, 3.534, 3.468, 3.405, 3.344, 3.286, 3.23, 3.176, 3.123, 3.542e-05, 2.763, 2.579},
            {0.0,  0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   4.936, 4.906, 4.833, 4.764, 4.698, 4.635, 4.574, 4.515,     4.458, 4.403,
             4.35, 4.298, 4.2, 4.107, 4.019, 3.935, 3.856, 3.78, 3.707, 3.638, 3.571, 3.507, 3.446, 3.387, 3.33,  3.275, 3.542e-05, 2.896, 2.703},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   5.159, 5.081, 5.007, 4.936, 4.868, 4.803, 4.741,     4.681, 4.622,
             4.566, 4.512, 4.407, 4.309, 4.216, 4.128, 4.044, 3.964, 3.887, 3.814, 3.744, 3.677, 3.612, 3.55,  3.49,  3.432, 3.542e-05, 3.035, 2.832},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   5.75, 5.662, 5.579, 5.499, 5.423, 5.35,      5.28,  5.212,
             5.147, 5.084, 4.964, 4.851, 4.744, 4.643, 4.547, 4.456, 4.369, 4.286, 4.206, 4.13, 4.056, 3.986, 3.918, 3.853, 3.542e-05, 3.404, 3.176},
            {0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   6.395, 6.296, 6.202, 6.112, 6.027,     5.945, 5.866,
             5.79, 5.717, 5.579, 5.449, 5.327, 5.211, 5.102, 4.998, 4.898, 4.804, 4.714, 4.627, 4.544, 4.464, 4.388, 4.314, 3.542e-05, 3.808, 3.552},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   7.098, 6.985, 6.879, 6.779,     6.683, 6.591,
             6.503, 6.418, 6.258, 6.108, 5.968, 5.836, 5.711, 5.593, 5.48, 5.373, 5.27, 5.172, 5.078, 4.988, 4.902, 4.819, 3.542e-05, 4.25,  3.962},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   7.861, 7.734, 7.615,     7.502, 7.395,
             7.292, 7.193, 7.008, 6.835, 6.674, 6.523, 6.38, 6.245, 6.118, 5.996, 5.88, 5.769, 5.663, 5.561, 5.464, 5.37,  3.542e-05, 4.732, 4.410},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   8.69,  8.547,     8.413, 8.286,
             8.166, 8.051, 7.835, 7.636, 7.451, 7.278, 7.115, 6.961, 6.816, 6.678, 6.547, 6.421, 6.302, 6.187, 6.078, 5.972, 3.542e-05, 5.257, 4.897},
            {0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   9.588,     9.428, 9.277,
             9.135, 9.0, 8.749, 8.519, 8.305, 8.106, 7.92, 7.745, 7.58, 7.423, 7.275, 7.133, 6.998, 6.87, 6.746, 6.628, 3.542e-05, 5.827, 5.425},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,       10.56, 10.38,
             10.21, 10.05, 9.759, 9.491, 9.244, 9.016, 8.803, 8.603, 8.415, 8.238, 8.069, 7.91, 7.758, 7.613, 7.474, 7.341, 3.542e-05, 6.445, 5.998},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   11.62,
             11.41, 11.22, 10.88, 10.56, 10.28, 10.01, 9.769, 9.541, 9.328, 9.126, 8.936, 8.756, 8.584, 8.421, 8.265, 8.116, 3.542e-05, 7.115, 6.618},
            {0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             12.75, 12.53, 12.11, 11.75, 11.41, 11.11, 10.83, 10.57, 10.32, 10.1, 9.88, 9.676, 9.483, 9.299, 9.124, 8.957, 3.542e-05, 7.84, 7.288},
            {0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 13.99, 13.49, 13.05, 12.67, 12.31, 11.99, 11.69, 11.41, 11.15, 10.91, 10.68, 10.46, 10.25, 10.06, 9.869, 3.542e-05, 8.623, 8.011},
            {0.0, 0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 16.75, 16.12, 15.58, 15.1, 14.66, 14.26, 13.9, 13.56, 13.25, 12.95, 12.67, 12.41, 12.16, 11.93, 3.542e-05, 10.38, 9.628},
            {0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 19.97, 19.17, 18.49, 17.89, 17.36, 16.87, 16.43, 16.02, 15.64, 15.28, 14.95, 14.63, 14.34, 3.542e-05, 12.42, 11.5},
            {0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 23.71, 22.7, 21.85, 21.1, 20.45, 19.85, 19.31, 18.81, 18.35, 17.93, 17.53, 17.15, 3.542e-05, 14.77, 13.65},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 28.07, 26.78, 25.71, 24.79, 23.97, 23.25, 22.59, 21.99, 21.44, 20.93, 20.45, 3.542e-05, 17.48, 16.12},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,  0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 33.16, 31.5, 30.15, 29.0, 28.0, 27.11, 26.31, 25.59, 24.92, 24.31, 3.542e-05, 20.6, 18.94},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 39.13, 36.97, 35.25, 33.82, 32.58, 31.5, 30.53, 29.65, 28.86, 3.542e-05, 24.19, 22.16},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 46.17, 43.33, 41.13, 39.33, 37.8, 36.47, 35.29, 34.24, 3.542e-05, 28.31, 25.84},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 54.54, 50.75, 47.92, 45.65, 43.75, 42.11, 40.68, 3.542e-05, 33.07, 30.03},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,  0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 64.64, 59.47, 55.78, 52.9, 50.53, 48.51, 3.542e-05, 38.55, 34.81},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,  0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 77.05, 69.8, 64.93, 61.24, 58.27, 3.542e-05, 44.92, 40.28},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 92.76, 82.18, 75.63, 70.87, 3.542e-05, 52.35, 46.54},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 113.6, 97.22, 88.27, 3.542e-05, 61.12, 53.76},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,       0.0,  0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,       0.0,  0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,       0.0,  0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,       0.0,  0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 143.9, 115.8, 3.542e-05, 71.6, 62.15},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 201.8, 3.542e-05, 84.38, 71.99},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.542e-05, 148.4, 115.1},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.542e-05, 201.7, 144.2},
            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0,   0.0,  0.0,
             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.542e-05, 270.9, 177.8},
        }};

    //*****************************************************************************

    template <size_t NumOfTemps, size_t NumOfConcs>
    void InterpDefValuesForGlycolConc(
        EnergyPlusData &state,
        const std::array<Real64, NumOfConcs> &RawConcData,                         // concentrations for raw data
        const std::array<std::array<Real64, NumOfTemps>, NumOfConcs> &RawPropData, // raw property data (concentration, temperature)
        Real64 Concentration,                                                      // concentration of actual fluid mix
        Array1D<Real64> &InterpData                                                // interpolated output data at proper concentration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   June 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // The purpose of this subroutine is to find the values for the property
        // data at a particular concentration from default data that is at "generic"
        // concentrations.  This is then returned to the main get routine and
        // then used later in the program to find values at various temperatures.
        // The ultimate purpose of this is to avoid double interpolation during
        // the simulation.  Since concentration does not change during the simulation,
        // there is no reason to do a double interpolation every time a property
        // value is needed.

        // METHODOLOGY EMPLOYED:
        // Fairly straight forward--find the two concentrations between which
        // the actual concentration falls and then interpolate the property
        // data using standard linear interpolation.  Note that data is stored
        // in the format: std::array[Concentration][Temperature]

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr Real64 ConcToler(0.0001); // Some reasonable value for comparisons
        static constexpr std::string_view RoutineName("InterpDefValuesForGlycolConc: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        size_t HiIndex;    // index on the high side of the concentration
        Real64 InterpFrac; // intermediate value for interpolations
        size_t LoopC;      // loop counter for concentration
        size_t LoopT;      // loop counter for temperature

        // First, find where the actual concentration falls between the concentration data.
        // Then, interpolate if necessary.
        if (Concentration < RawConcData[0]) { // Concentration too low
            ShowWarningError(state,
                             format("{}Glycol concentration out of range for data (too low), concentration = {:.3R}", RoutineName, Concentration));
            ShowContinueError(state, "Check your data or the definition of your glycols in the GlycolConcentrations input");
            ShowContinueError(state, "Property data set to data for lowest concentration entered");
            InterpData = RawPropData[0][0];
        } else if (Concentration > RawConcData[NumOfConcs - 1]) { // Concentration too high
            ShowWarningError(state,
                             format("{}Glycol concentration out of range for data (too high), concentration = {:.3R}", RoutineName, Concentration));
            ShowContinueError(state, "Check your data or the definition of your glycols in the GlycolConcentrations input");
            ShowContinueError(state, "Property data set to data for highest concentration entered");
            InterpData = RawPropData[NumOfConcs - 1][0];
        } else {                      // Concentration somewhere between lowest and highest point--interpolate
            HiIndex = NumOfConcs - 1; // Default to highest concentration - 1, since std::arrays start at 0
            for (LoopC = 1; LoopC < NumOfConcs - 1; ++LoopC) {
                if (Concentration <= RawConcData[LoopC]) {
                    HiIndex = LoopC;
                    break; // LoopC DO loop
                }
            }
            if (std::abs(RawConcData[HiIndex] - RawConcData[HiIndex - 1]) >= ConcToler) {
                InterpFrac = (RawConcData[HiIndex] - Concentration) / (RawConcData[HiIndex] - RawConcData[HiIndex - 1]);
                for (LoopT = 0; LoopT < NumOfTemps; ++LoopT) {
                    if ((RawPropData[HiIndex][LoopT] < ConcToler) || (RawPropData[HiIndex - 1][LoopT] < ConcToler)) {
                        // One of the two values is zero--so we cannot interpolate for this point (assign to zero)
                        InterpData(LoopT + 1) = 0.0;
                    } else {
                        InterpData(LoopT + 1) =
                            RawPropData[HiIndex][LoopT] - (InterpFrac * (RawPropData[HiIndex][LoopT] - RawPropData[HiIndex - 1][LoopT]));
                    }
                }
            } else { // user has input data for concentrations that are too close or repeated, this must be fixed
                ShowFatalError(state,
                               std::string{RoutineName} + "concentration values too close or data repeated, check your fluid property input data");
            }
        }
    }

    //*****************************************************************************

    void InterpValuesForGlycolConc(EnergyPlusData &state,
                                   int const NumOfConcs,               // number of concentrations (dimension of raw data)
                                   int const NumOfTemps,               // number of temperatures (dimension of raw data)
                                   const Array1D<Real64> &RawConcData, // concentrations for raw data
                                   Array2S<Real64> const RawPropData,  // raw property data (temperature,concentration)
                                   Real64 const Concentration,         // concentration of actual fluid mix
                                   Array1D<Real64> &InterpData         // interpolated output data at proper concentration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   June 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // The purpose of this subroutine is to find the values for the property
        // data at a particular concentration from default data that is at "generic"
        // concentrations.  This is then returned to the main get routine and
        // then used later in the program to find values at various temperatures.
        // The ultimate purpose of this is to avoid double interpolation during
        // the simulation.  Since concentration does not change during the simulation,
        // there is no reason to do a double interpolation every time a property
        // value is needed.

        // METHODOLOGY EMPLOYED:
        // Fairly straight forward--find the two concentrations between which
        // the actual concentration falls and then interpolate the property
        // data using standard linear interpolation.  Note that data is stored
        // in the format: 2dArray(Temperature,Concentration).  Temperature
        // data is not needed here since we are only interpolating to eliminate
        // the concentration as a variable (it really isn't one during the
        // simulation).

        // REFERENCES:
        // GetFluidPropertiesData--subroutine forces user to input data in
        // order of increasing concentration.  This is assumed in this subroutine.

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr Real64 ConcToler(0.0001); // Some reasonable value for comparisons
        static constexpr std::string_view RoutineName("InterpValuesForGlycolConc: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HiIndex;       // index on the high side of the concentration
        Real64 InterpFrac; // intermediate value for interpolations
        int LoopC;         // loop counter for concentration
        int LoopT;         // loop counter for temperature

        // First, find where the actual concentration falls between the concentration data.
        // Then, interpolate if necessary.
        if (Concentration < RawConcData(1)) { // Concentration too low
            ShowWarningError(state,
                             format("{}Glycol concentration out of range for data (too low), concentration = {:.3R}", RoutineName, Concentration));
            ShowContinueError(state, "Check your data or the definition of your glycols in the GlycolConcentrations input");
            ShowContinueError(state, "Property data set to data for lowest concentration entered");
            InterpData = RawPropData(1, _);
        } else if (Concentration > RawConcData(NumOfConcs)) { // Concentration too high
            ShowWarningError(state,
                             format("{}Glycol concentration out of range for data (too high), concentration = {:.3R}", RoutineName, Concentration));
            ShowContinueError(state, "Check your data or the definition of your glycols in the GlycolConcentrations input");
            ShowContinueError(state, "Property data set to data for highest concentration entered");
            InterpData = RawPropData(NumOfConcs, _);
        } else {                  // Concentration somewhere between lowest and highest point--interpolate
            HiIndex = NumOfConcs; // Default to highest concentration
            for (LoopC = 2; LoopC <= NumOfConcs - 1; ++LoopC) {
                if (Concentration <= RawConcData(LoopC)) {
                    HiIndex = LoopC;
                    break; // LoopC DO loop
                }
            }
            if (std::abs(RawConcData(HiIndex) - RawConcData(HiIndex - 1)) >= ConcToler) {
                InterpFrac = (RawConcData(HiIndex) - Concentration) / (RawConcData(HiIndex) - RawConcData(HiIndex - 1));
                for (LoopT = 1; LoopT <= NumOfTemps; ++LoopT) {
                    if ((RawPropData(HiIndex, LoopT) < ConcToler) || (RawPropData(HiIndex - 1, LoopT) < ConcToler)) {
                        InterpData(LoopT) = 0.0;
                    } else {
                        InterpData(LoopT) =
                            RawPropData(HiIndex, LoopT) - (InterpFrac * (RawPropData(HiIndex, LoopT) - RawPropData(HiIndex - 1, LoopT)));
                    }
                }
            } else { // user has input data for concentrations that are too close or repeated, this must be fixed
                ShowFatalError(state,
                               std::string{RoutineName} + "concentration values too close or data repeated, check your fluid property input data");
            }
        }
    }

    //*****************************************************************************

    void InitializeGlycolTempLimits(EnergyPlusData &state, bool &ErrorsFound) // set to true if errors found here
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine sets up the min/max temperature limits for the glycol properties.
        // Most properties requested (e.g., Specific Heat) must be > 0 but the tables may
        // be set up for symmetry and not be limited to just valid values.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int GlycolNum;
        int IndexNum;
        bool Failure;

        for (GlycolNum = 1; GlycolNum <= state.dataFluidProps->NumOfGlycols; ++GlycolNum) {
            if (state.dataFluidProps->GlycolData(GlycolNum).CpDataPresent) {
                // check for lowest non-zero value by referencing temp data
                for (IndexNum = 1; IndexNum <= state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts; ++IndexNum) {
                    if (state.dataFluidProps->GlycolData(GlycolNum).CpValues(IndexNum) <= 0.0) continue;
                    state.dataFluidProps->GlycolData(GlycolNum).CpLowTempIndex = IndexNum;
                    state.dataFluidProps->GlycolData(GlycolNum).CpLowTempValue = state.dataFluidProps->GlycolData(GlycolNum).CpTemps(IndexNum);
                    break;
                }
                // check for highest non-zero value by referencing temp data
                for (IndexNum = state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts; IndexNum >= 1; --IndexNum) {
                    if (state.dataFluidProps->GlycolData(GlycolNum).CpValues(IndexNum) <= 0.0) continue;
                    state.dataFluidProps->GlycolData(GlycolNum).CpHighTempIndex = IndexNum;
                    state.dataFluidProps->GlycolData(GlycolNum).CpHighTempValue = state.dataFluidProps->GlycolData(GlycolNum).CpTemps(IndexNum);
                    break;
                }
            }
            if (state.dataFluidProps->GlycolData(GlycolNum).RhoDataPresent) {
                // check for lowest non-zero value by referencing temp data
                for (IndexNum = 1; IndexNum <= state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts; ++IndexNum) {
                    if (state.dataFluidProps->GlycolData(GlycolNum).RhoValues(IndexNum) <= 0.0) continue;
                    state.dataFluidProps->GlycolData(GlycolNum).RhoLowTempIndex = IndexNum;
                    state.dataFluidProps->GlycolData(GlycolNum).RhoLowTempValue = state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(IndexNum);
                    break;
                }
                // check for highest non-zero value  by referencing temp data
                for (IndexNum = state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts; IndexNum >= 1; --IndexNum) {
                    if (state.dataFluidProps->GlycolData(GlycolNum).RhoValues(IndexNum) <= 0.0) continue;
                    state.dataFluidProps->GlycolData(GlycolNum).RhoHighTempIndex = IndexNum;
                    state.dataFluidProps->GlycolData(GlycolNum).RhoHighTempValue = state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(IndexNum);
                    break;
                }
            }
            if (state.dataFluidProps->GlycolData(GlycolNum).CondDataPresent) {
                // check for lowest non-zero value by referencing temp data
                for (IndexNum = 1; IndexNum <= state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts; ++IndexNum) {
                    if (state.dataFluidProps->GlycolData(GlycolNum).CondValues(IndexNum) <= 0.0) continue;
                    state.dataFluidProps->GlycolData(GlycolNum).CondLowTempIndex = IndexNum;
                    state.dataFluidProps->GlycolData(GlycolNum).CondLowTempValue = state.dataFluidProps->GlycolData(GlycolNum).CondTemps(IndexNum);
                    break;
                }
                // check for highest non-zero value  by referencing temp data
                for (IndexNum = state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts; IndexNum >= 1; --IndexNum) {
                    if (state.dataFluidProps->GlycolData(GlycolNum).CondValues(IndexNum) <= 0.0) continue;
                    state.dataFluidProps->GlycolData(GlycolNum).CondHighTempIndex = IndexNum;
                    state.dataFluidProps->GlycolData(GlycolNum).CondHighTempValue = state.dataFluidProps->GlycolData(GlycolNum).CondTemps(IndexNum);
                    break;
                }
            }
            if (state.dataFluidProps->GlycolData(GlycolNum).ViscDataPresent) {
                // check for lowest non-zero value by referencing temp data
                for (IndexNum = 1; IndexNum <= state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts; ++IndexNum) {
                    if (state.dataFluidProps->GlycolData(GlycolNum).ViscValues(IndexNum) <= 0.0) continue;
                    state.dataFluidProps->GlycolData(GlycolNum).ViscLowTempIndex = IndexNum;
                    state.dataFluidProps->GlycolData(GlycolNum).ViscLowTempValue = state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(IndexNum);
                    break;
                }
                // check for highest non-zero value  by referencing temp data
                for (IndexNum = state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts; IndexNum >= 1; --IndexNum) {
                    if (state.dataFluidProps->GlycolData(GlycolNum).ViscValues(IndexNum) <= 0.0) continue;
                    state.dataFluidProps->GlycolData(GlycolNum).ViscHighTempIndex = IndexNum;
                    state.dataFluidProps->GlycolData(GlycolNum).ViscHighTempValue = state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(IndexNum);
                    break;
                }
            }
            Failure = false;
            // Check to see that all are set to non-zero
            if (state.dataFluidProps->GlycolData(GlycolNum).CpDataPresent) {
                if (state.dataFluidProps->GlycolData(GlycolNum).CpLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->GlycolData(GlycolNum).CpHighTempIndex == 0) Failure = true;
            }
            if (state.dataFluidProps->GlycolData(GlycolNum).RhoDataPresent) {
                if (state.dataFluidProps->GlycolData(GlycolNum).RhoLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->GlycolData(GlycolNum).RhoHighTempIndex == 0) Failure = true;
            }
            if (state.dataFluidProps->GlycolData(GlycolNum).CondDataPresent) {
                if (state.dataFluidProps->GlycolData(GlycolNum).CondLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->GlycolData(GlycolNum).CondHighTempIndex == 0) Failure = true;
            }
            if (state.dataFluidProps->GlycolData(GlycolNum).ViscDataPresent) {
                if (state.dataFluidProps->GlycolData(GlycolNum).ViscLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->GlycolData(GlycolNum).ViscHighTempIndex == 0) Failure = true;
            }
            if (Failure) {
                ShowSevereError(state,
                                "InitializeGlycolTempLimits: Required values for Glycol=" + state.dataFluidProps->GlycolData(GlycolNum).Name +
                                    " are all zeroes for some data types.");
                ErrorsFound = true;
            }
        }
    }

    //*****************************************************************************

    void InitializeRefrigerantLimits(EnergyPlusData &state, bool &ErrorsFound) // set to true if errors found here
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine sets up the min/max limits (usually temperature and/or pressure)
        // for the refrigerant properties.
        // Most properties requested (e.g., Specific Heat) must be > 0 but the tables may
        // be set up for symmetry and not be limited to just valid values.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RefrigNum;
        int IndexNum;
        bool Failure;

        for (RefrigNum = 1; RefrigNum <= state.dataFluidProps->NumOfRefrigerants; ++RefrigNum) {
            for (IndexNum = 1; IndexNum <= state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints; ++IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).PsValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).PsLowPresIndex = IndexNum;
                state.dataFluidProps->RefrigData(RefrigNum).PsLowPresValue = state.dataFluidProps->RefrigData(RefrigNum).PsValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).PsLowTempValue = state.dataFluidProps->RefrigData(RefrigNum).PsTemps(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).PsLowTempIndex = IndexNum;
                break;
            }
            for (IndexNum = state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints; IndexNum >= 1; --IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).PsValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).PsHighPresIndex = IndexNum;
                state.dataFluidProps->RefrigData(RefrigNum).PsHighPresValue = state.dataFluidProps->RefrigData(RefrigNum).PsValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).PsHighTempValue = state.dataFluidProps->RefrigData(RefrigNum).PsTemps(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).PsHighTempIndex = IndexNum;
                break;
            }
            for (IndexNum = 1; IndexNum <= state.dataFluidProps->RefrigData(RefrigNum).NumHPoints; ++IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).HfValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).HfLowTempValue = state.dataFluidProps->RefrigData(RefrigNum).HfValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).HfLowTempIndex = IndexNum;
                break;
            }
            for (IndexNum = state.dataFluidProps->RefrigData(RefrigNum).NumHPoints; IndexNum >= 1; --IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).HfValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).HfHighTempValue = state.dataFluidProps->RefrigData(RefrigNum).HfValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).HfHighTempIndex = IndexNum;
                break;
            }
            for (IndexNum = 1; IndexNum <= state.dataFluidProps->RefrigData(RefrigNum).NumHPoints; ++IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).HfgValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).HfgLowTempValue = state.dataFluidProps->RefrigData(RefrigNum).HfgValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).HfgLowTempIndex = IndexNum;
                break;
            }
            for (IndexNum = state.dataFluidProps->RefrigData(RefrigNum).NumHPoints; IndexNum >= 1; --IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).HfgValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).HfgHighTempValue = state.dataFluidProps->RefrigData(RefrigNum).HfgValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).HfgHighTempIndex = IndexNum;
                break;
            }
            for (IndexNum = 1; IndexNum <= state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints; ++IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).CpfValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).CpfLowTempValue = state.dataFluidProps->RefrigData(RefrigNum).CpfValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).CpfLowTempIndex = IndexNum;
                break;
            }
            for (IndexNum = state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints; IndexNum >= 1; --IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).CpfValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).CpfHighTempValue = state.dataFluidProps->RefrigData(RefrigNum).CpfValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).CpfHighTempIndex = IndexNum;
                break;
            }
            for (IndexNum = 1; IndexNum <= state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints; ++IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).CpfgValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).CpfgLowTempValue = state.dataFluidProps->RefrigData(RefrigNum).CpfgValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).CpfgLowTempIndex = IndexNum;
                break;
            }
            for (IndexNum = state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints; IndexNum >= 1; --IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).CpfgValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).CpfgHighTempValue = state.dataFluidProps->RefrigData(RefrigNum).CpfgValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).CpfgHighTempIndex = IndexNum;
                break;
            }
            for (IndexNum = 1; IndexNum <= state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints; ++IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).RhofValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).RhofLowTempValue = state.dataFluidProps->RefrigData(RefrigNum).RhofValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).RhofLowTempIndex = IndexNum;
                break;
            }
            for (IndexNum = state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints; IndexNum >= 1; --IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).RhofValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).RhofHighTempValue = state.dataFluidProps->RefrigData(RefrigNum).RhofValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).RhofHighTempIndex = IndexNum;
                break;
            }
            for (IndexNum = 1; IndexNum <= state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints; ++IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).RhofgValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).RhofgLowTempValue = state.dataFluidProps->RefrigData(RefrigNum).RhofgValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).RhofgLowTempIndex = IndexNum;
                break;
            }
            for (IndexNum = state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints; IndexNum >= 1; --IndexNum) {
                if (state.dataFluidProps->RefrigData(RefrigNum).RhofgValues(IndexNum) <= 0.0) continue;
                state.dataFluidProps->RefrigData(RefrigNum).RhofgHighTempValue = state.dataFluidProps->RefrigData(RefrigNum).RhofgValues(IndexNum);
                state.dataFluidProps->RefrigData(RefrigNum).RhofgHighTempIndex = IndexNum;
                break;
            }
            Failure = false;
            // Check to see that all are set to non-zero
            if (state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints > 0) {
                if (state.dataFluidProps->RefrigData(RefrigNum).PsLowPresIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).PsLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).PsHighPresIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).PsHighTempIndex == 0) Failure = true;
            }
            if (state.dataFluidProps->RefrigData(RefrigNum).NumHPoints > 0) {
                if (state.dataFluidProps->RefrigData(RefrigNum).HfLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).HfgLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).HfHighTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).HfgHighTempIndex == 0) Failure = true;
            }
            if (state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints > 0) {
                if (state.dataFluidProps->RefrigData(RefrigNum).CpfLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).CpfgLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).CpfHighTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).CpfgHighTempIndex == 0) Failure = true;
            }
            if (state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints > 0) {
                if (state.dataFluidProps->RefrigData(RefrigNum).RhofLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).RhofgLowTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).RhofHighTempIndex == 0) Failure = true;
                if (state.dataFluidProps->RefrigData(RefrigNum).RhofgHighTempIndex == 0) Failure = true;
            }
            if (Failure) {
                ShowSevereError(state,
                                "InitializeRefrigerantLimits: Required values for Refrigerant=" + state.dataFluidProps->RefrigData(RefrigNum).Name +
                                    " are all zeroes for some data types.");
                ErrorsFound = true;
            }
        }
    }

    //*****************************************************************************

    void ReportAndTestGlycols(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is written to report and test glycols through their range
        // of temperatures and make sure that proper values will be returned.

        // METHODOLOGY EMPLOYED:
        // Use internal structure as the temperature limits. Write output to the
        // debug output file.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr Real64 incr(10.0);
        static constexpr std::string_view RoutineName("ReportAndTestGlycols");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int GlycolNum;      // Loop Counter
        Real64 Temperature; // Temperature to drive values
        Real64 ReturnValue; // Values returned from glycol functions
        int Loop;           // Loop Counter
        int GlycolIndex;    // index used in routine / function calls, value is returned on first use (when index=0)

        state.dataFluidProps->GetInput = false; // input has already been gotten

        for (GlycolNum = 1; GlycolNum <= state.dataFluidProps->NumOfGlycols; ++GlycolNum) {
            GlycolIndex = 0; // used in routine calls -- value is returned when first 0
            // Lay out the basic values:
            if (state.dataFluidProps->GlycolData(GlycolNum).GlycolName != "") {
                print(state.files.debug,
                      "Glycol={}, Mixture fluid={}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).Name,
                      state.dataFluidProps->GlycolData(GlycolNum).GlycolName);
            } else {
                print(state.files.debug, "Glycol={}\n", state.dataFluidProps->GlycolData(GlycolNum).Name);
            }
            print(state.files.debug, "Concentration:,{:.2R}\n", state.dataFluidProps->GlycolData(GlycolNum).Concentration);
            if (state.dataFluidProps->GlycolData(GlycolNum).CpDataPresent) {
                print(state.files.debug,
                      "Specific Heat Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).CpLowTempValue,
                      state.dataFluidProps->GlycolData(GlycolNum).CpLowTempIndex,
                      state.dataFluidProps->GlycolData(GlycolNum).CpHighTempValue,
                      state.dataFluidProps->GlycolData(GlycolNum).CpHighTempIndex);
                print(state.files.debug, "{}", "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).CpTemps(Loop));
                }
                print(state.files.debug,
                      ",{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).CpTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts));
                print(state.files.debug, "{}", "Specific Heat:");
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).CpValues(Loop));
                }
                print(state.files.debug,
                      ",{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).CpValues(state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts));
            }
            if (state.dataFluidProps->GlycolData(GlycolNum).RhoDataPresent) {
                print(state.files.debug,
                      "Density Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).RhoLowTempValue,
                      state.dataFluidProps->GlycolData(GlycolNum).RhoLowTempIndex,
                      state.dataFluidProps->GlycolData(GlycolNum).RhoHighTempValue,
                      state.dataFluidProps->GlycolData(GlycolNum).RhoHighTempIndex);
                print(state.files.debug, "{}", "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(Loop));
                }
                print(state.files.debug,
                      ",{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts));
                print(state.files.debug, "{}", "Density:");
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).RhoValues(Loop));
                }
                print(state.files.debug,
                      ",{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts));
            }
            if (state.dataFluidProps->GlycolData(GlycolNum).CondDataPresent) {
                print(state.files.debug,
                      "Conductivity Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).CondLowTempValue,
                      state.dataFluidProps->GlycolData(GlycolNum).CondLowTempIndex,
                      state.dataFluidProps->GlycolData(GlycolNum).CondHighTempValue,
                      state.dataFluidProps->GlycolData(GlycolNum).CondHighTempIndex);
                print(state.files.debug, "{}", "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).CondTemps(Loop));
                }
                print(state.files.debug,
                      ",{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).CondTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts));
                print(state.files.debug, "{}", "Conductivity:");
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).CondValues(Loop));
                }
                print(state.files.debug,
                      ",{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).CondValues(state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts));
            }
            if (state.dataFluidProps->GlycolData(GlycolNum).ViscDataPresent) {
                print(state.files.debug,
                      "Viscosity Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).ViscLowTempValue,
                      state.dataFluidProps->GlycolData(GlycolNum).ViscLowTempIndex,
                      state.dataFluidProps->GlycolData(GlycolNum).ViscHighTempValue,
                      state.dataFluidProps->GlycolData(GlycolNum).ViscHighTempIndex);
                print(state.files.debug, "{}", "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(Loop));
                }
                print(state.files.debug,
                      ",{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts));
                print(state.files.debug, "{}", "Viscosity:");
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).ViscValues(Loop));
                }
                print(state.files.debug,
                      ",{}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).ViscValues(state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts));
            }
            // ============================================
            // Glycol Results, using out of bounds to out of bounds values in calling
            // ============================================

            // ========= Specific Heat from Temperatures
            print(state.files.debug, "Glycol={} **** Results ****\n", state.dataFluidProps->GlycolData(GlycolNum).Name);
            if (state.dataFluidProps->GlycolData(GlycolNum).CpDataPresent) {
                print(state.files.debug, "Specific Heat Results at Temperatures:");
                print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).CpTemps(1) - incr);

                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).CpTemps(Loop));
                    Temperature =
                        state.dataFluidProps->GlycolData(GlycolNum).CpTemps(Loop) +
                        (state.dataFluidProps->GlycolData(GlycolNum).CpTemps(Loop + 1) - state.dataFluidProps->GlycolData(GlycolNum).CpTemps(Loop)) /
                            2.0;
                    print(state.files.debug, ",{:.2R}", Temperature);
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->GlycolData(GlycolNum).CpTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts));
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).CpTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts) + incr);
                print(state.files.debug, "Specific Heat:");
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).CpTemps(1) - incr;
                ReturnValue = GetSpecificHeatGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts - 1; ++Loop) {
                    Temperature = state.dataFluidProps->GlycolData(GlycolNum).CpTemps(Loop);
                    ReturnValue =
                        GetSpecificHeatGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                    Temperature =
                        state.dataFluidProps->GlycolData(GlycolNum).CpTemps(Loop) +
                        (state.dataFluidProps->GlycolData(GlycolNum).CpTemps(Loop + 1) - state.dataFluidProps->GlycolData(GlycolNum).CpTemps(Loop)) /
                            2.0;
                    ReturnValue =
                        GetSpecificHeatGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                }
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).CpTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts);
                ReturnValue = GetSpecificHeatGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).CpTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCpTempPts) + incr;
                ReturnValue = GetSpecificHeatGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.2R}\n", ReturnValue);
            }

            // ========= Density from Temperatures
            if (state.dataFluidProps->GlycolData(GlycolNum).RhoDataPresent) {
                print(state.files.debug, "Density Results at Temperatures:");
                print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(1) - incr);
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(Loop));
                    Temperature =
                        state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(Loop) + (state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(Loop + 1) -
                                                                                      state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(Loop)) /
                                                                                         2.0;
                    print(state.files.debug, ",{:.2R}", Temperature);
                }
                print(state.files.debug,
                      ",{}",
                      state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts));
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts) + incr);
                print(state.files.debug, "Density:");
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(1) - incr;
                ReturnValue = GetDensityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.3R}", ReturnValue);
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts - 1; ++Loop) {
                    Temperature = state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(Loop);
                    ReturnValue = GetDensityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                    print(state.files.debug, ",{:.3R}", ReturnValue);
                    Temperature =
                        state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(Loop) + (state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(Loop + 1) -
                                                                                      state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(Loop)) /
                                                                                         2.0;
                    ReturnValue = GetDensityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                    print(state.files.debug, ",{:.3R}", ReturnValue);
                }
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts);
                ReturnValue = GetDensityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.3R}", ReturnValue);
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).RhoTemps(state.dataFluidProps->GlycolData(GlycolNum).NumRhoTempPts) + incr;
                ReturnValue = GetDensityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.3R}\n", ReturnValue);
            }

            // ========= Conductivity from Temperatures
            if (state.dataFluidProps->GlycolData(GlycolNum).CondDataPresent) {
                print(state.files.debug, "Conductivity Results at Temperatures:");
                print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).CondTemps(1) - incr);
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).CondTemps(Loop));
                    Temperature = state.dataFluidProps->GlycolData(GlycolNum).CondTemps(Loop) +
                                  (state.dataFluidProps->GlycolData(GlycolNum).CondTemps(Loop + 1) -
                                   state.dataFluidProps->GlycolData(GlycolNum).CondTemps(Loop)) /
                                      2.0;
                    print(state.files.debug, ",{:.2R}", Temperature);
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->GlycolData(GlycolNum).CondTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts));
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).CondTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts) + incr);
                print(state.files.debug, "Conductivity:");
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).CondTemps(1) - incr;
                ReturnValue = GetConductivityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.3R}", ReturnValue);
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts - 1; ++Loop) {
                    Temperature = state.dataFluidProps->GlycolData(GlycolNum).CondTemps(Loop);
                    ReturnValue =
                        GetConductivityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                    print(state.files.debug, ",{:.3R}", ReturnValue);
                    Temperature = state.dataFluidProps->GlycolData(GlycolNum).CondTemps(Loop) +
                                  (state.dataFluidProps->GlycolData(GlycolNum).CondTemps(Loop + 1) -
                                   state.dataFluidProps->GlycolData(GlycolNum).CondTemps(Loop)) /
                                      2.0;
                    ReturnValue =
                        GetConductivityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                    print(state.files.debug, ",{:.3R}", ReturnValue);
                }
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).CondTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts);
                ReturnValue = GetConductivityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.3R}", ReturnValue);
                Temperature =
                    state.dataFluidProps->GlycolData(GlycolNum).CondTemps(state.dataFluidProps->GlycolData(GlycolNum).NumCondTempPts) + incr;
                ReturnValue = GetConductivityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.3R}\n", ReturnValue);
            }

            // ========= Viscosity from Temperatures
            if (state.dataFluidProps->GlycolData(GlycolNum).ViscDataPresent) {
                print(state.files.debug, "Viscosity Results at Temperatures:");
                print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(1) - incr);
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(Loop));
                    Temperature = state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(Loop) +
                                  (state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(Loop + 1) -
                                   state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(Loop)) /
                                      2.0;
                    print(state.files.debug, ",{:.2R}", Temperature);
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts));
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts) + incr);
                print(state.files.debug, "Viscosity:");
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(1) - incr;
                ReturnValue = GetViscosityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.4R}", ReturnValue);
                for (Loop = 1; Loop <= state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts - 1; ++Loop) {
                    Temperature = state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(Loop);
                    ReturnValue = GetViscosityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                    print(state.files.debug, ",{:.4R}", ReturnValue);
                    Temperature = state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(Loop) +
                                  (state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(Loop + 1) -
                                   state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(Loop)) /
                                      2.0;
                    ReturnValue = GetViscosityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                    print(state.files.debug, ",{:.4R}", ReturnValue);
                }
                Temperature = state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts);
                ReturnValue = GetViscosityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.4R}", ReturnValue);
                Temperature =
                    state.dataFluidProps->GlycolData(GlycolNum).ViscTemps(state.dataFluidProps->GlycolData(GlycolNum).NumViscTempPts) + incr;
                ReturnValue = GetViscosityGlycol(state, state.dataFluidProps->GlycolData(GlycolNum).Name, Temperature, GlycolIndex, RoutineName);
                print(state.files.debug, ",{:.4R}\n", ReturnValue);
            }
        }
    }

    //*****************************************************************************

    void ReportAndTestRefrigerants(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008; only stub provided to satisfy calling programs.
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is written to report and test refrigerants through their range
        // of inputs (temperatures?) and make sure that proper values will be returned.

        // METHODOLOGY EMPLOYED:
        // Use internal structure as the range limits. Write output to the
        // debug output file.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr Real64 incr(10.0);
        constexpr Real64 Quality(1.0);
        static constexpr std::string_view RoutineName("ReportAndTestRefrigerants");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RefrigNum;      // Loop Counter
        Real64 Temperature; // Temperature to drive values
        Real64 ReturnValue; // Values returned from refrigerant functions
        int Loop;           // Loop Counter
        int Loop1;          // Loop Counter
        int RefrigIndex;

        state.dataFluidProps->GetInput = false; // input has already been gotten

        for (RefrigNum = 1; RefrigNum <= state.dataFluidProps->NumOfRefrigerants; ++RefrigNum) {
            RefrigIndex = 0; // used in routine calls -- value is returned when first 0
            // Lay out the basic values:
            if (!state.dataFluidProps->RefrigData(RefrigNum).Name.empty()) {
                print(state.files.debug, "Refrigerant={}", state.dataFluidProps->RefrigData(RefrigNum).Name);
            }
            if (state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints > 0) {
                print(state.files.debug,
                      "Saturation Pressures Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).PsLowTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).PsLowTempIndex,
                      state.dataFluidProps->RefrigData(RefrigNum).PsHighTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).PsHighTempIndex);
                print(state.files.debug, "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).PsTemps(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).PsTemps(state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints));
                print(state.files.debug, "Saturation Pressure:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).PsValues(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).PsValues(state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints));
            }
            if (state.dataFluidProps->RefrigData(RefrigNum).NumHPoints > 0) {
                print(state.files.debug,
                      "Enthalpy Saturated Fluid Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).HfLowTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).HfLowTempIndex,
                      state.dataFluidProps->RefrigData(RefrigNum).HfHighTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).HfHighTempIndex);
                print(state.files.debug, "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumHPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).HTemps(state.dataFluidProps->RefrigData(RefrigNum).NumHPoints));
                print(state.files.debug, "Enthalpy Saturated Fluid:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumHPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).HfValues(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).HfValues(state.dataFluidProps->RefrigData(RefrigNum).NumHPoints));
                print(state.files.debug,
                      "Enthalpy Saturated Fluid/Gas Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).HfgLowTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).HfgLowTempIndex,
                      state.dataFluidProps->RefrigData(RefrigNum).HfgHighTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).HfgHighTempIndex);

                print(state.files.debug, "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumHPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).HTemps(state.dataFluidProps->RefrigData(RefrigNum).NumHPoints));
                print(state.files.debug, "Enthalpy Saturated Fluid/Gas:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumHPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).HfgValues(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).HfgValues(state.dataFluidProps->RefrigData(RefrigNum).NumHPoints));
            }
            if (state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints > 0) {
                print(state.files.debug,
                      "Specific Heat Saturated Fluid Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).CpfLowTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).CpfLowTempIndex,
                      state.dataFluidProps->RefrigData(RefrigNum).CpfHighTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).CpfHighTempIndex);
                print(state.files.debug, "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).CpTemps(state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints));
                print(state.files.debug, "Specific Heat Saturated Fluid:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}\n", state.dataFluidProps->RefrigData(RefrigNum).CpfValues(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->RefrigData(RefrigNum).CpfValues(state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints));
                print(state.files.debug,
                      "Specific Heat Saturated Fluid/Gas Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).CpfgLowTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).CpfgLowTempIndex,
                      state.dataFluidProps->RefrigData(RefrigNum).CpfgHighTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).CpfgHighTempIndex);
                print(state.files.debug, "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).CpTemps(state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints));
                print(state.files.debug, "Specific Heat Saturated Fluid/Gas:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).CpfgValues(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).CpfgValues(state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints));
            }
            if (state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints > 0) {
                print(state.files.debug,
                      "Density Saturated Fluid Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).RhofLowTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).RhofLowTempIndex,
                      state.dataFluidProps->RefrigData(RefrigNum).RhofHighTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).RhofHighTempIndex);
                print(state.files.debug, "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints));
                print(state.files.debug, "Density Saturated Fluid:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).RhofValues(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->RefrigData(RefrigNum).RhofValues(state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints));
                print(state.files.debug,
                      "Density Saturated Fluid/Gas Data points:,Low Temperature=,{:.2R},Index=,{},High Temperature=,{:.2R},Index=,{}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).RhofgLowTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).RhofgLowTempIndex,
                      state.dataFluidProps->RefrigData(RefrigNum).RhofgHighTempValue,
                      state.dataFluidProps->RefrigData(RefrigNum).RhofgHighTempIndex);
                print(state.files.debug, "Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints));
                print(state.files.debug, "Density Saturated Fluid/Gas:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).RhofgValues(Loop));
                }
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).RhofgValues(state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints));
            }

            if (state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts > 0 && state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts > 0) {
                print(state.files.debug,
                      "Superheated Gas Fluid Data points:,NumTemperaturePoints=,{},NumPressurePoints=,{}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts,
                      state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts);
                print(state.files.debug, "Superheated Temperatures:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.3R}", state.dataFluidProps->RefrigData(RefrigNum).SHTemps(Loop));
                }
                print(state.files.debug,
                      ",{:.3R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).SHTemps(state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts));
                print(state.files.debug, "Superheated Pressures:");
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts - 1; ++Loop) {
                    print(state.files.debug, ",{:.3R}", state.dataFluidProps->RefrigData(RefrigNum).SHPress(Loop));
                }
                print(state.files.debug,
                      ",{:.3R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).SHPress(state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts));
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts; ++Loop) {
                    print(state.files.debug, "Superheated Pressure:#{}={:.2R}\n", Loop, state.dataFluidProps->RefrigData(RefrigNum).SHPress(Loop));
                    print(state.files.debug, "Enthalpy Superheated Gas:");
                    for (Loop1 = 1; Loop1 <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts - 1; ++Loop1) {
                        print(state.files.debug, ",{:.3R}", state.dataFluidProps->RefrigData(RefrigNum).HshValues(Loop, Loop1));
                    }
                    print(state.files.debug,
                          ",{:.3R}\n",
                          state.dataFluidProps->RefrigData(RefrigNum).HshValues(Loop, state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts));
                }
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts; ++Loop) {
                    print(state.files.debug, "Superheated Pressure:#{}={:.2R}\n", Loop, state.dataFluidProps->RefrigData(RefrigNum).SHPress(Loop));
                    print(state.files.debug, "Density Superheated Gas:");
                    for (Loop1 = 1; Loop1 <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts - 1; ++Loop1) {
                        print(state.files.debug, ",{:.3R}", state.dataFluidProps->RefrigData(RefrigNum).RhoshValues(Loop, Loop1));
                    }
                    print(state.files.debug,
                          ",{:.3R}\n",
                          state.dataFluidProps->RefrigData(RefrigNum).RhoshValues(Loop, state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts));
                }
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts; ++Loop) {
                    print(state.files.debug, "Superheated Temperature:#{}={:.2R}\n", Loop, state.dataFluidProps->RefrigData(RefrigNum).SHTemps(Loop));
                    print(state.files.debug, "Enthalpy Superheated Gas:");
                    for (Loop1 = 1; Loop1 <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts - 1; ++Loop1) {
                        print(state.files.debug, ",{:.3R}", state.dataFluidProps->RefrigData(RefrigNum).HshValues(Loop1, Loop));
                    }
                    print(state.files.debug,
                          ",{:.3R}\n",
                          state.dataFluidProps->RefrigData(RefrigNum).HshValues(state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts, Loop));
                }
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperTempPts; ++Loop) {
                    print(state.files.debug, "Superheated Temperature:#{}={:.2R}\n", Loop, state.dataFluidProps->RefrigData(RefrigNum).SHTemps(Loop));
                    print(state.files.debug, "Density Superheated Gas:");
                    for (Loop1 = 1; Loop1 <= state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts - 1; ++Loop1) {
                        print(state.files.debug, ",{:.3R}", state.dataFluidProps->RefrigData(RefrigNum).RhoshValues(Loop1, Loop));
                    }
                    print(
                        state.files.debug,
                        ",{:.3R}\n",
                        state.dataFluidProps->RefrigData(RefrigNum).RhoshValues(state.dataFluidProps->RefrigData(RefrigNum).NumSuperPressPts, Loop));
                }
            }

            // ============================================
            // Refrigeration Results, using out of bounds to out of bounds values in calling
            // ============================================

            // ========= Pressure from Temperatures
            print(state.files.debug, "Refrigerant={} **** Results ****\n", state.dataFluidProps->RefrigData(RefrigNum).Name);
            if (state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints > 0) {
                print(state.files.debug, "Pressure Results at Temperatures:");
                print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).PsTemps(1) - incr);
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).PsTemps(Loop));
                    Temperature =
                        state.dataFluidProps->RefrigData(RefrigNum).PsTemps(Loop) +
                        (state.dataFluidProps->RefrigData(RefrigNum).PsTemps(Loop + 1) - state.dataFluidProps->RefrigData(RefrigNum).PsTemps(Loop)) /
                            2.0;
                    print(state.files.debug, ",{:.2R}", Temperature);
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->RefrigData(RefrigNum).PsTemps(state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints));
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).PsTemps(state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints) + incr);
                print(state.files.debug, "Saturated Pressures:");
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).PsTemps(1) - incr;
                ReturnValue = GetSatPressureRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints - 1; ++Loop) {
                    Temperature = state.dataFluidProps->RefrigData(RefrigNum).PsTemps(Loop);
                    ReturnValue =
                        GetSatPressureRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, RefrigIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                    Temperature =
                        state.dataFluidProps->RefrigData(RefrigNum).PsTemps(Loop) +
                        (state.dataFluidProps->RefrigData(RefrigNum).PsTemps(Loop + 1) - state.dataFluidProps->RefrigData(RefrigNum).PsTemps(Loop)) /
                            2.0;
                    ReturnValue =
                        GetSatPressureRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, RefrigIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                }
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).PsTemps(state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints);
                ReturnValue = GetSatPressureRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).PsTemps(state.dataFluidProps->RefrigData(RefrigNum).NumPsPoints) + incr;
                ReturnValue = GetSatPressureRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}\n", ReturnValue);
            }

            // ========= Enthalpy from Temperatures
            if (state.dataFluidProps->RefrigData(RefrigNum).NumHPoints > 0) {
                print(state.files.debug, "Enthalpy Results at Temperatures:");
                print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).HTemps(1) - incr);
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumHPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop));
                    Temperature =
                        state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop) +
                        (state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop + 1) - state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop)) /
                            2.0;
                    print(state.files.debug, ",{:.2R}", Temperature);
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->RefrigData(RefrigNum).HTemps(state.dataFluidProps->RefrigData(RefrigNum).NumHPoints));
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).HTemps(state.dataFluidProps->RefrigData(RefrigNum).NumHPoints) + incr);
                print(state.files.debug, "Saturated Enthalpy:");
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).HTemps(1) - incr;
                ReturnValue =
                    GetSatEnthalpyRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumHPoints - 1; ++Loop) {
                    Temperature = state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop);
                    ReturnValue =
                        GetSatEnthalpyRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                    Temperature =
                        state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop) +
                        (state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop + 1) - state.dataFluidProps->RefrigData(RefrigNum).HTemps(Loop)) /
                            2.0;
                    ReturnValue =
                        GetSatEnthalpyRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                }
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).HTemps(state.dataFluidProps->RefrigData(RefrigNum).NumHPoints);
                ReturnValue =
                    GetSatEnthalpyRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).HTemps(state.dataFluidProps->RefrigData(RefrigNum).NumHPoints) + incr;
                ReturnValue =
                    GetSatEnthalpyRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}\n", ReturnValue);
            }

            // ========= Specific Heat from Temperatures
            if (state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints > 0) {
                print(state.files.debug, "Specific Heat Results at Temperatures:");
                print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).CpTemps(1) - incr);
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop));
                    Temperature =
                        state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop) +
                        (state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop + 1) - state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop)) /
                            2.0;
                    print(state.files.debug, ",{:.2R}", Temperature);
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->RefrigData(RefrigNum).CpTemps(state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints));
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).CpTemps(state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints) + incr);
                print(state.files.debug, "Saturated Specific Heat:");
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).CpTemps(1) - incr;
                ReturnValue =
                    GetSatSpecificHeatRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints - 1; ++Loop) {
                    Temperature = state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop);
                    ReturnValue = GetSatSpecificHeatRefrig(
                        state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                    Temperature =
                        state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop) +
                        (state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop + 1) - state.dataFluidProps->RefrigData(RefrigNum).CpTemps(Loop)) /
                            2.0;
                    ReturnValue = GetSatSpecificHeatRefrig(
                        state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                }
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).CpTemps(state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints);
                ReturnValue =
                    GetSatSpecificHeatRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).CpTemps(state.dataFluidProps->RefrigData(RefrigNum).NumCpPoints) + incr;
                ReturnValue =
                    GetSatSpecificHeatRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}\n", ReturnValue);
            }

            // ========= Density from Temperatures
            if (state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints > 0) {
                print(state.files.debug, "Density Results at Temperatures:");
                print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(1) - incr);
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints - 1; ++Loop) {
                    print(state.files.debug, ",{:.2R}", state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop));
                    Temperature =
                        state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop) + (state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop + 1) -
                                                                                      state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop)) /
                                                                                         2.0;
                    print(state.files.debug, ",{:.2R}", Temperature);
                }
                print(state.files.debug,
                      ",{:.2R}",
                      state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints));
                print(state.files.debug,
                      ",{:.2R}\n",
                      state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints) + incr);
                print(state.files.debug, "Saturated Density:");
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(1) - incr;
                ReturnValue =
                    GetSatDensityRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                for (Loop = 1; Loop <= state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints - 1; ++Loop) {
                    Temperature = state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop);
                    ReturnValue =
                        GetSatDensityRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                    Temperature =
                        state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop) + (state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop + 1) -
                                                                                      state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(Loop)) /
                                                                                         2.0;
                    ReturnValue =
                        GetSatDensityRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                    print(state.files.debug, ",{:.2R}", ReturnValue);
                }
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints);
                ReturnValue =
                    GetSatDensityRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}", ReturnValue);
                Temperature = state.dataFluidProps->RefrigData(RefrigNum).RhoTemps(state.dataFluidProps->RefrigData(RefrigNum).NumRhoPoints) + incr;
                ReturnValue =
                    GetSatDensityRefrig(state, state.dataFluidProps->RefrigData(RefrigNum).Name, Temperature, Quality, RefrigIndex, RoutineName);
                print(state.files.debug, ",{:.2R}\n", ReturnValue);
            }
        }
    }

    //*****************************************************************************

    Real64 GetSatPressureRefrig(EnergyPlusData &state,
                                std::string_view const Refrigerant, // carries in substance name
                                Real64 const Temperature,           // actual temperature given as input
                                int &RefrigIndex,                   // Index to Refrigerant Properties
                                std::string_view const CalledFrom   // routine this function was called from (error messages)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   24 May 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This finds the saturation pressure for given temperature.

        // METHODOLOGY EMPLOYED:
        // Calls FindArrayIndex to find indices either side of requested temperature
        // and linearly interpolates the corresponding saturation pressure values.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSatPressureRefrig: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int HiTempIndex;        // index value of next highest Temperature from table
        int LoTempIndex;        // index value of next lowest Temperature from table
        int RefrigNum;          // index for refrigerant under consideration
        Real64 TempInterpRatio; // ratio to interpolate in temperature domain
        // error counters and dummy string
        bool ErrorFlag; // error flag for current call

        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetSatPressureRefrig", "properties", CalledFrom);
        }

        ErrorFlag = false;

        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(
                    state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetSatPressureRefrig", "properties", CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        // determine array indices for
        LoTempIndex = FindArrayIndex(Temperature, refrig.PsTemps, refrig.PsLowTempIndex, refrig.PsHighTempIndex);
        HiTempIndex = LoTempIndex + 1;

        // check for out of data bounds problems
        if (LoTempIndex == 0) {
            ReturnValue = refrig.PsValues(refrig.PsLowTempIndex);
            ErrorFlag = true;
        } else if (HiTempIndex > refrig.PsHighTempIndex) {
            ReturnValue = refrig.PsValues(refrig.PsHighTempIndex);
            ErrorFlag = true;
        } else {
            // find interpolation ratio w.r.t temperature
            TempInterpRatio = (Temperature - refrig.PsTemps(LoTempIndex)) / (refrig.PsTemps(HiTempIndex) - refrig.PsTemps(LoTempIndex));

            // apply final linear interpolation
            ReturnValue = refrig.PsValues(LoTempIndex) + TempInterpRatio * (refrig.PsValues(HiTempIndex) - refrig.PsValues(LoTempIndex));
        }

        if (!state.dataGlobal->WarmupFlag && ErrorFlag) {
            ++state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatTempErrCount;
            // send warning
            if (state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatTempErrCount <= state.dataFluidProps->RefrigerantErrorLimitTest) {
                ShowSevereMessage(state,
                                  std::string{RoutineName} + "Saturation temperature is out of range for refrigerant [" +
                                      state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name + "] supplied data: **");
                ShowContinueError(state,
                                  format("...Called From:{}, supplied data range=[{:.2R},{:.2R}]",
                                         CalledFrom,
                                         refrig.PsTemps(refrig.PsLowTempIndex),
                                         refrig.PsTemps(refrig.PsHighTempIndex)));
                ShowContinueError(
                    state, format("...Supplied Refrigerant Temperature={:.2R} Returned saturated pressure value = {:.0R}", Temperature, ReturnValue));
                ShowContinueErrorTimeStamp(state, "");
            }
            ShowRecurringSevereErrorAtEnd(state,
                                          std::string{RoutineName} + "Saturation temperature is out of range for refrigerant [" +
                                              state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name + "] supplied data: **",
                                          state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatTempErrIndex,
                                          Temperature,
                                          Temperature,
                                          _,
                                          "{C}",
                                          "{C}");
        }

        return ReturnValue;
    }

    //*****************************************************************************

    Real64 GetSatTemperatureRefrig(EnergyPlusData &state,
                                   std::string_view const Refrigerant, // carries in substance name
                                   Real64 const Pressure,              // actual temperature given as input
                                   int &RefrigIndex,                   // Index to Refrigerant Properties
                                   std::string_view const CalledFrom   // routine this function was called from (error messages)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   24 May 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This finds the saturation temperature for given pressure.

        // METHODOLOGY EMPLOYED:
        // Calls FindArrayIndex to find indices either side of requested pressure
        // and linearly interpolates the corresponding saturation temperature values.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSatTemperatureRefrig: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int HiPresIndex;        // index value of next highest Temperature from table
        int LoPresIndex;        // index value of next lowest Temperature from table
        int RefrigNum;          // index for refrigerant under consideration
        Real64 PresInterpRatio; // ratio to interpolate in temperature domain
        // error counters and dummy string
        bool ErrorFlag; // error flag for current call

        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetSatTemperatureRefrig", "properties", CalledFrom);
        }

        ErrorFlag = false;

        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(state,
                                             state.dataFluidProps->NumOfRefrigerants,
                                             RefrigNum,
                                             true,
                                             Refrigerant,
                                             "GetSatTemperatureRefrig",
                                             "properties",
                                             CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        // get the array indices
        LoPresIndex = FindArrayIndex(Pressure, refrig.PsValues, refrig.PsLowPresIndex, refrig.PsHighPresIndex);
        HiPresIndex = LoPresIndex + 1;

        // check for out of data bounds problems
        if (LoPresIndex == 0) {
            ReturnValue = refrig.PsTemps(refrig.PsLowPresIndex);
            ErrorFlag = true;
        } else if (HiPresIndex > refrig.PsHighPresIndex) {
            ReturnValue = refrig.PsTemps(refrig.PsHighPresIndex);
            ErrorFlag = true;
        } else {
            // find interpolation ratio w.r.t temperature
            PresInterpRatio = (Pressure - refrig.PsValues(LoPresIndex)) / (refrig.PsValues(HiPresIndex) - refrig.PsValues(LoPresIndex));

            // apply final linear interpolation
            ReturnValue = refrig.PsTemps(LoPresIndex) + PresInterpRatio * (refrig.PsTemps(HiPresIndex) - refrig.PsTemps(LoPresIndex));
        }

        if (!state.dataGlobal->WarmupFlag && ErrorFlag) {
            ++state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatPressErrCount;
            // send warning
            if (state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatPressErrCount <= state.dataFluidProps->RefrigerantErrorLimitTest) {
                ShowSevereMessage(state,
                                  std::string{RoutineName} + "Saturation pressure is out of range for refrigerant [" +
                                      state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name + "] supplied data: **");
                ShowContinueError(state,
                                  format("...Called From:{}, supplied data range=[{:.0R},{:.0R}]",
                                         CalledFrom,
                                         refrig.PsValues(refrig.PsLowPresIndex),
                                         refrig.PsValues(refrig.PsHighPresIndex)));
                ShowContinueError(
                    state, format("...Supplied Refrigerant Pressure={:.0R} Returned saturated temperature value ={:.2R}", Pressure, ReturnValue));
                ShowContinueErrorTimeStamp(state, "");
            }
            ShowRecurringSevereErrorAtEnd(state,
                                          std::string{RoutineName} + "Saturation pressure is out of range for refrigerant [" +
                                              state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name + "] supplied data: **",
                                          state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatPressErrIndex,
                                          Pressure,
                                          Pressure,
                                          _,
                                          "{Pa}",
                                          "{Pa}");
        }
        return ReturnValue;
    }

    //*****************************************************************************

    Real64 GetSatEnthalpyRefrig(EnergyPlusData &state,
                                std::string_view const Refrigerant, // carries in substance name
                                Real64 const Temperature,           // actual temperature given as input
                                Real64 const Quality,               // actual quality given as input
                                int &RefrigIndex,                   // Index to Refrigerant Properties
                                std::string_view const CalledFrom   // routine this function was called from (error messages)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mike Turner
        //       DATE WRITTEN   10 December 99
        //       MODIFIED       Rick Strand (April 2000, May 2000)
        //                      Simon Rees (May 2002)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This finds enthalpy for given temperature and a quality under the vapor dome.
        // This fucntion is only called with a valid refrigerant and quality between 0 and 1.

        // METHODOLOGY EMPLOYED:
        // Calls GetInterpolatedSatProp to linearly interpolate between the saturated
        // liquid  and vapour enthalpies according to the given quality.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSatEnthalpyRefrig");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int RefrigNum; // index for refrigerant under consideration

        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, RoutineName, "properties", CalledFrom);
        }

        if ((Quality < 0.0) || (Quality > 1.0)) {
            ShowSevereError(state, fmt::format("{}: Refrigerant \"{}\", invalid quality, called from {}", RoutineName, Refrigerant, CalledFrom));
            ShowContinueError(state, format("Saturated refrigerant quality must be between 0 and 1, entered value=[{:.4R}].", Quality));
            ShowFatalError(state, "Program terminates due to preceding condition.");
        }

        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(
                    state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, RoutineName, "properties", CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        // Apply linear interpolation function
        return GetInterpolatedSatProp(
            state, Temperature, refrig.HTemps, refrig.HfValues, refrig.HfgValues, Quality, CalledFrom, refrig.HfLowTempIndex, refrig.HfHighTempIndex);
    }

    //*****************************************************************************

    Real64 GetSatDensityRefrig(EnergyPlusData &state,
                               std::string_view const Refrigerant, // carries in substance name
                               Real64 const Temperature,           // actual temperature given as input
                               Real64 const Quality,               // actual quality given as input
                               int &RefrigIndex,                   // Index to Refrigerant Properties
                               std::string_view const CalledFrom   // routine this function was called from (error messages)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mike Turner
        //       DATE WRITTEN   10 December 99
        //       MODIFIED       Rick Strand (April 2000, May 2000)
        //                      Simon Rees (May 2002); Kenneth Tang (Jan 2004)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This finds density for given temperature and a quality under the vapor dome.
        // This function is only called with a valid refrigerant and quality between 0 and 1.

        // METHODOLOGY EMPLOYED:
        // Calls GetInterpolatedSatProp to linearly interpolate between the saturated
        // liquid  and vapour densities according to the given quality.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSatDensityRefrig: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        int RefrigNum;          // index for refrigerant under consideration
        int HiTempIndex;        // array index for temp above input temp
        int LoTempIndex;        // array index for temp below input temp
        Real64 LoSatProp;       // Sat. prop. at lower temp & given quality
        Real64 HiSatProp;       // Sat. prop. at higher temp & given quality
        Real64 TempInterpRatio; // ratio to interpolate in temperature domain
        bool ErrorFlag;         // error flag for current call

        // error counters and dummy string

        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetSatDensityRefrig", "properties", CalledFrom);
        }

        if ((Quality < 0.0) || (Quality > 1.0)) {
            ShowSevereError(state, fmt::format("{}Refrigerant \"{}\", invalid quality, called from {}", RoutineName, Refrigerant, CalledFrom));
            ShowContinueError(state, format("Saturated density quality must be between 0 and 1, entered value=[{:.4R}].", Quality));
            ShowFatalError(state, "Program terminates due to preceding condition.");
        }

        // Find which refrigerant (index) is being requested and then determine
        // where the temperature is within the temperature array
        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(
                    state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetSatDensityRefrig", "properties", CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        ErrorFlag = false;

        LoTempIndex = FindArrayIndex(Temperature, refrig.RhoTemps, refrig.RhofLowTempIndex, refrig.RhofHighTempIndex);
        HiTempIndex = LoTempIndex + 1;

        // Error check to make sure the temperature is not out of bounds
        if (LoTempIndex == 0) {
            // Give the lowest density value if the temperature is below than the minimum
            // temperature in the refrigerant table
            ReturnValue = 1.0 / refrig.RhofValues(refrig.RhofLowTempIndex) +
                          Quality * (1.0 / refrig.RhofgValues(refrig.RhofLowTempIndex) - 1.0 / refrig.RhofValues(refrig.RhofLowTempIndex));
            ReturnValue = 1.0 / ReturnValue;
            ErrorFlag = true;
        } else if (HiTempIndex > refrig.RhofHighTempIndex) {
            // Give the highest density value if the temperature is higher than the maximum
            // temperature in the refrigerant table
            ReturnValue = 1.0 / refrig.RhofValues(refrig.RhofHighTempIndex) +
                          Quality * (1.0 / refrig.RhofgValues(refrig.RhofHighTempIndex) - 1.0 / refrig.RhofValues(refrig.RhofHighTempIndex));
            ReturnValue = 1.0 / ReturnValue;
            ErrorFlag = true;
        } else { // Okay

            // Calculate the specific volume for the lower temperature index based on linear
            // interpolation of the quality
            LoSatProp =
                1.0 / refrig.RhofValues(LoTempIndex) + Quality * (1.0 / refrig.RhofgValues(LoTempIndex) - 1.0 / refrig.RhofValues(LoTempIndex));

            // Calculate the specific volume for the higher temperature index based on linear
            // interpolation of the quality
            HiSatProp =
                1.0 / refrig.RhofValues(HiTempIndex) + Quality * (1.0 / refrig.RhofgValues(HiTempIndex) - 1.0 / refrig.RhofValues(HiTempIndex));

            // Find interpolation ratio in temperature direction
            TempInterpRatio = (Temperature - refrig.RhoTemps(LoTempIndex)) / (refrig.RhoTemps(HiTempIndex) - refrig.RhoTemps(LoTempIndex));

            // Apply final linear interpolation to find the specific volume
            ReturnValue = LoSatProp + TempInterpRatio * (HiSatProp - LoSatProp);
            // Convert the specific volume to density
            ReturnValue = 1.0 / ReturnValue;
        }

        if (!state.dataGlobal->WarmupFlag && ErrorFlag) {
            ++state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatTempDensityErrCount;
            // send warning
            if (state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatTempDensityErrCount <= state.dataFluidProps->RefrigerantErrorLimitTest) {
                ShowSevereMessage(state,
                                  std::string{RoutineName} + "Saturation temperature is out of range for refrigerant [" +
                                      state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name + "] supplied data: **");
                ShowContinueError(state,
                                  format("...Called From:{}, supplied data range=[{:.2R},{:.2R}]",
                                         CalledFrom,
                                         refrig.RhoTemps(refrig.RhofLowTempIndex),
                                         refrig.RhoTemps(refrig.RhofHighTempIndex)));
                ShowContinueError(
                    state, format("...Supplied Refrigerant Temperature={:.2R} Returned saturated density value ={:.2R}", Temperature, ReturnValue));
                ShowContinueErrorTimeStamp(state, "");
            }
            ShowRecurringSevereErrorAtEnd(state,
                                          std::string{RoutineName} + "Saturation temperature is out of range for refrigerant [" +
                                              state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name + "] supplied data: **",
                                          state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatTempDensityErrIndex,
                                          Temperature,
                                          Temperature,
                                          _,
                                          "{C}",
                                          "{C}");
        }
        return ReturnValue;
    }

    //*****************************************************************************

    Real64 GetSatSpecificHeatRefrig(EnergyPlusData &state,
                                    std::string_view const Refrigerant, // carries in substance name
                                    Real64 const Temperature,           // actual temperature given as input
                                    Real64 const Quality,               // actual quality given as input
                                    int &RefrigIndex,                   // Index to Refrigerant Properties
                                    std::string_view const CalledFrom   // routine this function was called from (error messages)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mike Turner
        //       DATE WRITTEN   10 December 99
        //       MODIFIED       Rick Strand (April 2000, May 2000)
        //                      Simon Rees (May 2002)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This finds specific heat for given temperature and a quality under the vapor dome.
        // This fucntion is only called with a valid refrigerant and quality between 0 and 1.

        // METHODOLOGY EMPLOYED:
        // Calls GetInterpolatedSatProp to linearly interpolate between the saturated
        // liquid  and vapour specific heats according to the given quality.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSatSpecificHeatRefrig: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int RefrigNum; // index for refrigerant under consideration

        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetSatSpecificHeatRefrig", "properties", CalledFrom);
        }

        if ((Quality < 0.0) || (Quality > 1.0)) {
            ShowSevereError(state, fmt::format("{}Refrigerant \"{}\", invalid quality, called from {}", RoutineName, Refrigerant, CalledFrom));
            ShowContinueError(state, format("Saturated density quality must be between 0 and 1, entered value=[{:.4R}].", Quality));
            ShowFatalError(state, "Program terminates due to preceding condition.");
        }

        // Find which refrigerant (index) is being requested and then determine
        // where the temperature is within the temperature array
        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(state,
                                             state.dataFluidProps->NumOfRefrigerants,
                                             RefrigNum,
                                             true,
                                             Refrigerant,
                                             "GetSatSpecificHeatRefrig",
                                             "properties",
                                             CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        // Apply linear interpolation function
        ReturnValue = GetInterpolatedSatProp(state,
                                             Temperature,
                                             refrig.CpTemps,
                                             refrig.CpfValues,
                                             refrig.CpfgValues,
                                             Quality,
                                             CalledFrom,
                                             refrig.CpfLowTempIndex,
                                             refrig.CpfHighTempIndex);

        return ReturnValue;
    }

    //*****************************************************************************

    Real64 GetSupHeatEnthalpyRefrig(EnergyPlusData &state,
                                    std::string_view const Refrigerant, // carries in substance name
                                    Real64 const Temperature,           // actual temperature given as input
                                    Real64 const Pressure,              // actual pressure given as input
                                    int &RefrigIndex,                   // Index to Refrigerant Properties
                                    std::string_view const CalledFrom   // routine this function was called from (error messages)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mike Turner
        //       DATE WRITTEN   10 December 99
        //       MODIFIED       Rick Strand (April 2000, May 2000)
        //       MODIFIED       Simon Rees (May 2002)
        //       RE-ENGINEERED  N/A

        // PURPOSE OF THIS SUBROUTINE:
        // Performs linear interpolation between pressures and temperatures and
        // returns enthalpy values.  Works only in superheated region.

        // METHODOLOGY EMPLOYED:
        // Double linear interpolation is used with enthalpy values at four
        // pressure/temperature input points surrounding the given temperature
        // and pressure argument values.
        // With enthalpy data it is assumed that zero values in the data are in
        // the saturated region. Hence, values near the saturation line are
        // approximated using the saturation value instead of the zero data value.
        // points completely in the saturation region are given the saturation value
        // at the given temperature. Points at the upper limits of pressure/temperature
        // have the pressure/temperature capped. Warnings are given if the point
        // is not clearly in the bounds of the superheated data.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSupHeatEnthalpyRefrig: ");
        static constexpr std::string_view RoutineNameNoSpace("GetSupHeatEnthalpyRefrig:");
        static constexpr std::string_view RoutineNameNoColon("GetSupHeatEnthalpyRefrig");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 PressInterpRatio; // Interpolation factor w.r.t pressure
        Real64 TempInterpRatio;  // Interpolation factor w.r.t temperature
        Real64 EnthalpyHigh;     // Enthalpy value at interpolated pressure and high temperature
        Real64 EnthalpyLow;      // Enthalpy value at interpolated pressure and low temperature
        Real64 LoTempLoEnthalpy; // Enthalpy value at low pressure and low temperature
        Real64 LoTempHiEnthalpy; // Enthalpy value at high pressure and low temperature
        Real64 HiTempLoEnthalpy; // Enthalpy value at low pressure and high temperature
        Real64 HiTempHiEnthalpy; // Enthalpy value at high pressure and high temperature

        int HiTempIndex;  // high temperature index value
        int HiPressIndex; // high pressure index value
        int LoPressIndex; // low index value of Pressure from table
        int RefrigNum;    // index for refrigerant under consideration
        int TempIndex;    // low index value of Temperature from table

        // error counters and dummy string
        int ErrCount;             // error counter for current call
        int CurTempRangeErrCount; // error counter for current call
        int CurPresRangeErrCount; // error counter for current call

        // see if data is there
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, RoutineNameNoColon, "properties", CalledFrom);
        }

        ErrCount = 0;
        CurTempRangeErrCount = 0;
        CurPresRangeErrCount = 0;

        // Find which refrigerant (index) is being requested and then determine
        // where the temperature and pressure are within the temperature and
        // pressure arrays, respectively
        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(
                    state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, RoutineNameNoColon, "properties", CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        TempIndex = FindArrayIndex(Temperature, refrig.SHTemps, 1, refrig.NumSuperTempPts);
        LoPressIndex = FindArrayIndex(Pressure, refrig.SHPress, 1, refrig.NumSuperPressPts);

        // check temperature data range and attempt to cap if necessary
        if ((TempIndex > 0) && (TempIndex < refrig.NumSuperTempPts)) { // in range
            HiTempIndex = TempIndex + 1;
            TempInterpRatio = (Temperature - refrig.SHTemps(TempIndex)) / (refrig.SHTemps(HiTempIndex) - refrig.SHTemps(TempIndex));
        } else if (TempIndex < 1) {
            ++CurTempRangeErrCount;
            ++ErrCount;
            TempIndex = 1;
            HiTempIndex = TempIndex;
            TempInterpRatio = 0.0;
        } else { // out of range
            ++CurTempRangeErrCount;
            ++ErrCount;
            // FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
            HiTempIndex = TempIndex;
            TempInterpRatio = 0.0;
        }

        // check pressure data range and attempt to cap if necessary
        if ((LoPressIndex > 0) && (LoPressIndex < refrig.NumSuperPressPts)) { // in range
            HiPressIndex = LoPressIndex + 1;
            PressInterpRatio = (Pressure - refrig.SHPress(LoPressIndex)) / (refrig.SHPress(HiPressIndex) - refrig.SHPress(LoPressIndex));
        } else if (LoPressIndex < 1) {
            ++CurPresRangeErrCount;
            ++ErrCount;
            // FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
            LoPressIndex = 1;
            HiPressIndex = LoPressIndex;
            PressInterpRatio = 0.0;
        } else { // out of range
            ++CurPresRangeErrCount;
            ++ErrCount;
            HiPressIndex = LoPressIndex;
            PressInterpRatio = 0.0;
        }

        // get interpolation point values
        LoTempLoEnthalpy = refrig.HshValues(LoPressIndex, TempIndex);
        LoTempHiEnthalpy = refrig.HshValues(HiPressIndex, TempIndex);
        HiTempLoEnthalpy = refrig.HshValues(LoPressIndex, HiTempIndex);
        HiTempHiEnthalpy = refrig.HshValues(HiPressIndex, HiTempIndex);

        // to give reasonable interpolation near saturation reset any point with zero value
        // in table to saturation value
        if (LoTempLoEnthalpy <= 0.0) {
            LoTempLoEnthalpy = GetSatEnthalpyRefrig(state, Refrigerant, Temperature, 1.0, RefrigNum, RoutineNameNoColon);
        }
        if (LoTempHiEnthalpy <= 0.0) {
            LoTempHiEnthalpy = GetSatEnthalpyRefrig(state, Refrigerant, Temperature, 1.0, RefrigNum, RoutineNameNoColon);
        }
        if (HiTempLoEnthalpy <= 0.0) {
            HiTempLoEnthalpy = GetSatEnthalpyRefrig(state, Refrigerant, Temperature, 1.0, RefrigNum, RoutineNameNoColon);
        }
        if (HiTempHiEnthalpy <= 0.0) {
            HiTempHiEnthalpy = GetSatEnthalpyRefrig(state, Refrigerant, Temperature, 1.0, RefrigNum, RoutineNameNoColon);
        }

        // interpolate w.r.t. pressure
        EnthalpyLow = PressInterpRatio * LoTempHiEnthalpy + (1.0 - PressInterpRatio) * LoTempLoEnthalpy;

        EnthalpyHigh = PressInterpRatio * HiTempHiEnthalpy + (1.0 - PressInterpRatio) * HiTempLoEnthalpy;

        // interpolate w.r.t. temperature
        ReturnValue = TempInterpRatio * EnthalpyHigh + (1.0 - TempInterpRatio) * EnthalpyLow;

        // Check to see if all data is at zero. In this case we are completely
        // inside the saturation dome. Best thing we can do is return saturation value
        if ((refrig.HshValues(LoPressIndex, TempIndex) <= 0.0) && (refrig.HshValues(HiPressIndex, TempIndex) <= 0.0) &&
            (refrig.HshValues(LoPressIndex, HiTempIndex) <= 0.0) && (refrig.HshValues(HiPressIndex, HiTempIndex) <= 0.0)) {
            ++state.dataFluidProps->SatErrCountGetSupHeatEnthalpyRefrig;
            // set return value
            ReturnValue = GetSatEnthalpyRefrig(state, Refrigerant, Temperature, 1.0, RefrigNum, fmt::format("{}{}", RoutineNameNoSpace, CalledFrom));
            // send warning
            if (!state.dataGlobal->WarmupFlag) {
                state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupEnthalpyErrCount +=
                    state.dataFluidProps->SatErrCountGetSupHeatEnthalpyRefrig;
                // send warning
                if (state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatTempDensityErrCount <= state.dataFluidProps->RefrigerantErrorLimitTest) {
                    ShowWarningMessage(state,
                                       std::string{RoutineName} + "Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                           "] is saturated at the given conditions, saturated enthalpy at given temperature returned. **");
                    ShowContinueError(state, fmt::format("...Called From:{}", CalledFrom));
                    ShowContinueError(state, format("Refrigerant temperature = {:.2R}", Temperature));
                    ShowContinueError(state, format("Refrigerant pressure = {:.0R}", Pressure));
                    ShowContinueError(state, format("Returned Enthalpy value = {:.3R}", ReturnValue));
                    ShowContinueErrorTimeStamp(state, "");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + "Refrigerant [" +
                                                   state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                                   "] saturated at the given conditions **",
                                               state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupEnthalpyErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }
            return ReturnValue;
        }

        if (!state.dataGlobal->WarmupFlag) {
            // some checks...
            if (ErrCount > 0) {
                // send temp range error if flagged
                state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupEnthalpyTempErrCount += CurTempRangeErrCount;
                if (CurTempRangeErrCount > 0 && state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupEnthalpyTempErrCount <=
                                                    state.dataFluidProps->RefrigerantErrorLimitTest) {
                    ShowWarningMessage(state,
                                       std::string{RoutineName} + "Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                           "] Temperature is out of range for superheated refrigerant enthalpy: values capped **");
                    ShowContinueError(state, fmt::format(" Called From:{}", CalledFrom));
                    ShowContinueErrorTimeStamp(state, "");
                }
                if (CurTempRangeErrCount > 0) {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   std::string{RoutineName} + "Refrigerant [" +
                                                       state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                                       "] Temperature is out of range for superheated refrigerant enthalpy: values capped **",
                                                   state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupEnthalpyTempErrIndex,
                                                   Temperature,
                                                   Temperature,
                                                   _,
                                                   "{C}",
                                                   "{C}");
                }

                // send pressure range error if flagged
                state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupEnthalpyPresErrCount += CurPresRangeErrCount;
                if (CurPresRangeErrCount > 0 && state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupEnthalpyPresErrCount <=
                                                    state.dataFluidProps->RefrigerantErrorLimitTest) {
                    ShowWarningMessage(state,
                                       std::string{RoutineName} + "Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                           "] Pressure is out of range for superheated refrigerant enthalpy: values capped **");
                    ShowContinueError(state, fmt::format(" Called From:{}", CalledFrom));
                    ShowContinueErrorTimeStamp(state, "");
                }
                if (CurPresRangeErrCount > 0) {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   std::string{RoutineName} + "Refrigerant [" +
                                                       state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                                       "] Pressure is out of range for superheated refrigerant enthalpy: values capped **",
                                                   state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupEnthalpyPresErrIndex,
                                                   Pressure,
                                                   Pressure,
                                                   _,
                                                   "{Pa}",
                                                   "{Pa}");
                }
            } // end error checking
        }

        return ReturnValue;
    }

    //*****************************************************************************

    Real64 GetSupHeatPressureRefrig(EnergyPlusData &state,
                                    std::string const &Refrigerant,   // carries in substance name
                                    Real64 const Temperature,         // actual temperature given as input
                                    Real64 const Enthalpy,            // actual enthalpy given as input
                                    int &RefrigIndex,                 // Index to Refrigerant Properties
                                    std::string_view const CalledFrom // routine this function was called from (error messages)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Simon Rees (May 2002)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Performs linear interpolation between enthalpy and temperatures and
        // returns pressure values.  Works only in superheated region.

        // METHODOLOGY EMPLOYED:
        // Double linear interpolation is used with pressure values at four
        // enthalpy/temperature input points surrounding the given temperature
        // and enthalpy argument values.
        // All enthalpies have to be calculated at the given temperature before a
        // search is made for the data adjacent to the given enthalpy. Linear interpolation
        // using the enthalpy data is used to interpolate the correspondng pressures.
        // Temperatures and enthalpies outside the bounds of the available data are capped
        // and warnings given. For enthlpys lower than the saturated vapour value at the
        // given temperature result in the saturation pressure being returned (calls to
        // GetSatEnthalpy and GetSatPressure are made.)

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETERS:
        // the enthalpy calculated from the pressure found
        static constexpr std::string_view RoutineName("GetSupHeatPressureRefrig: ");
        static constexpr std::string_view RoutineNameNoSpace("GetSupHeatPressureRefrig:");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 EnthalpyCheck;   // recalculates enthalpy based on calculated pressure
        Real64 EnthalpyHigh;    // Enthalpy value at interpolated pressure and high temperature
        Real64 EnthalpyLow;     // Enthalpy value at interpolated pressure and low temperature
        Real64 EnthalpyMax;     // Enthalpy value at interpolated pressure and high temperature
        Real64 EnthalpyMin;     // Enthalpy value at interpolated pressure and low temperature
        Real64 SatEnthalpy;     // Saturated vapour enthalpy
        Real64 TempInterpRatio; // Interpolation ratio w.r.t temperature
        Real64 EnthInterpRatio; // Interpolation ratio w.r.t enthalpy

        int finish; // index of high end of enthalpy values
        int start;  // index of high end of enthalpy values
        int Loop;   // DO loop counter
        int middle; // mid-point for interval halving

        int RefrigNum;    // index for refrigerant under consideration
        int LoTempStart;  // lower non-zero index of enthalpy values at lower temp.
        int LoTempFinish; // upper non-zero index of enthalpy values at lower temp.
        int HiTempStart;  // lower non-zero index of enthalpy values at higher temp.
        int HiTempFinish; // upper non-zero index of enthalpy values at higher temp.
        int TempStart;    // corrected lower non-zero index of enthalpy values
        int TempFinish;   // corrected upper non-zero index of enthalpy values

        int LoTempIndex;     // Index value of lower temperature from data
        int HiTempIndex;     // Index value of higher temperature from data
        int LoEnthalpyIndex; // Index value of lower enthalpy from data
        int HiEnthalpyIndex; // Index value of higher enthalpy from data

        // error counters and dummy string
        int ErrCount;                 // error counter for current call
        int CurTempRangeErrCount;     // error counter for current call
        int CurEnthalpyRangeErrCount; // error counter for current call
        int CurSatErrCount;           // error counter for current call

        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetSupHeatPressureRefrig", "properties", CalledFrom);
        }

        ErrCount = 0;
        CurTempRangeErrCount = 0;
        CurEnthalpyRangeErrCount = 0;
        CurSatErrCount = 0;

        // Find which refrigerant (index) is being requested and then determine
        // where the temperature is within the temperature array
        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(state,
                                             state.dataFluidProps->NumOfRefrigerants,
                                             RefrigNum,
                                             true,
                                             Refrigerant,
                                             "GetSupHeatPressureRefrig",
                                             "properties",
                                             CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        LoTempIndex = FindArrayIndex(Temperature, refrig.SHTemps, 1, refrig.NumSuperTempPts);
        HiTempIndex = LoTempIndex + 1;

        // check temperature data range and attempt to cap if necessary
        if ((LoTempIndex > 0) && (LoTempIndex < refrig.NumSuperTempPts)) { // in range
            HiTempIndex = LoTempIndex + 1;
        } else if (LoTempIndex < 1) { // below lower bound
            ++CurTempRangeErrCount;
            LoTempIndex = 1;
            HiTempIndex = LoTempIndex;
        } else { // out of range
            ++CurTempRangeErrCount;
            HiTempIndex = LoTempIndex;
        }

        // check for lowest non-zero value in lower temp data
        LoTempStart = refrig.NumSuperPressPts;
        for (Loop = 1; Loop <= refrig.NumSuperPressPts; ++Loop) {
            if (refrig.HshValues(Loop, LoTempIndex) > 0.0) {
                LoTempStart = Loop;
                break;
            }
        }
        // check for highest non-zero value in lower temp data
        LoTempFinish = 1;
        for (Loop = refrig.NumSuperPressPts; Loop >= 1; --Loop) {
            if (refrig.HshValues(Loop, LoTempIndex) <= 0.0) {
                LoTempFinish = Loop;
                // EXIT
            }
        }
        // check for lowest non-zero value in high temp data
        HiTempStart = refrig.NumSuperPressPts;
        for (Loop = 1; Loop <= refrig.NumSuperPressPts; ++Loop) {
            if (refrig.HshValues(Loop, HiTempIndex) > 0.0) {
                HiTempStart = Loop;
                break;
            }
        }

        // check for highest non-zero value in high temp data
        HiTempFinish = 1;
        for (Loop = refrig.NumSuperPressPts; Loop >= 1; --Loop) {
            if (refrig.HshValues(Loop, HiTempIndex) <= 0.0) {
                HiTempFinish = Loop;
            }
        }

        // find bounds of both hi and lo temp data
        TempStart = max(LoTempStart, HiTempStart);
        TempFinish = min(LoTempFinish, HiTempFinish);
        // calculate interpolation ratio w.r.t temperature
        // This ratio is used to find enthalpies at the given temperature
        TempInterpRatio = (Temperature - refrig.SHTemps(LoTempIndex)) / (refrig.SHTemps(HiTempIndex) - refrig.SHTemps(LoTempIndex));

        // search for array index by bisection
        start = TempStart; // set the bounds
        finish = TempFinish;

        // find the bounds of the enthalpy data available
        EnthalpyMax = max(refrig.HshValues(TempStart, LoTempIndex), refrig.HshValues(TempStart, HiTempIndex));
        EnthalpyMin = min(refrig.HshValues(TempFinish, LoTempIndex), refrig.HshValues(TempFinish, HiTempIndex));
        // get saturated enthalpy for checking
        SatEnthalpy = GetSatEnthalpyRefrig(state, Refrigerant, Temperature, 1.0, RefrigNum, fmt::format("{}{}", RoutineNameNoSpace, CalledFrom));

        // make some checks on the data before interpolating
        if (Enthalpy < SatEnthalpy) {
            // flag error
            ++CurSatErrCount;
            ++ErrCount;
            // return sat pressure at this temperature
            ReturnValue = GetSatPressureRefrig(state, Refrigerant, Temperature, RefrigNum, fmt::format("{}{}", RoutineNameNoSpace, CalledFrom));

        } else if (EnthalpyMax < Enthalpy || EnthalpyMin > Enthalpy) {
            // out of range error
            ++CurEnthalpyRangeErrCount;
            ++ErrCount;
            if (Enthalpy > EnthalpyMax) {
                // return min pressure
                ReturnValue = refrig.SHPress(HiTempStart);
            } else {
                // return max pressure
                ReturnValue = refrig.SHPress(LoTempFinish);
            }
        } else {
            // go ahead and search
            while ((finish - start) > 1) {
                middle = (finish + start) / 2;

                // calc enthalpy at middle index for given temperature
                EnthalpyCheck = refrig.HshValues(middle, LoTempIndex) +
                                TempInterpRatio * (refrig.HshValues(middle, HiTempIndex) - refrig.HshValues(middle, LoTempIndex));

                if (Enthalpy < EnthalpyCheck) {
                    start = middle;
                } else {
                    finish = middle;
                }
            }
            LoEnthalpyIndex = start;
            HiEnthalpyIndex = start + 1;

            // calculate enthalpies adjacent specified enthalpy at given temperature
            EnthalpyLow = refrig.HshValues(LoEnthalpyIndex, LoTempIndex) +
                          TempInterpRatio * (refrig.HshValues(LoEnthalpyIndex, HiTempIndex) - refrig.HshValues(LoEnthalpyIndex, LoTempIndex));

            EnthalpyHigh = refrig.HshValues(HiEnthalpyIndex, LoTempIndex) +
                           TempInterpRatio * (refrig.HshValues(HiEnthalpyIndex, HiTempIndex) - refrig.HshValues(HiEnthalpyIndex, LoTempIndex));
            // calculate an interpolation ratio
            EnthInterpRatio = (Enthalpy - EnthalpyLow) / (EnthalpyHigh - EnthalpyLow);
            // apply this interpolation ratio to find the final pressure
            ReturnValue = refrig.SHPress(LoEnthalpyIndex) + EnthInterpRatio * (refrig.SHPress(HiEnthalpyIndex) - refrig.SHPress(LoEnthalpyIndex));
        }

        if (!state.dataGlobal->WarmupFlag) {
            // ** make error checks **
            if (ErrCount > 0) {
                // send near saturation warning if flagged
                state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupPressureErrCount += CurSatErrCount;
                // send warning
                if (state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupPressureErrCount <= state.dataFluidProps->RefrigerantErrorLimitTest) {
                    ShowSevereMessage(
                        state,
                        std::string{RoutineName} + "Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                            "] is saturated at the given enthalpy and temperature, saturated enthalpy at given temperature returned. **");
                    ShowContinueError(state, fmt::format("...Called From:{}", CalledFrom));
                    ShowContinueError(state, format("Refrigerant temperature = {:.2R}", Temperature));
                    ShowContinueError(state, format("Refrigerant Enthalpy = {:.3R}", Enthalpy));
                    ShowContinueError(state, format("Returned Pressure value = {:.0R}", ReturnValue));
                    ShowContinueErrorTimeStamp(state, "");
                }
                if (CurSatErrCount > 0) {
                    ShowRecurringSevereErrorAtEnd(state,
                                                  std::string{RoutineName} + "Refrigerant [" +
                                                      state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                                      "] saturated at the given enthalpy and temperature **",
                                                  state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupPressureErrIndex,
                                                  ReturnValue,
                                                  ReturnValue,
                                                  _,
                                                  "{Pa}",
                                                  "{Pa}");
                }

                // send temp range error if flagged
                state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupPressureTempErrCount += CurTempRangeErrCount;
                if (CurTempRangeErrCount > 0 && state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupPressureTempErrCount <=
                                                    state.dataFluidProps->RefrigerantErrorLimitTest) {
                    ShowWarningMessage(state,
                                       std::string{RoutineName} + "Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                           "] Temperature is out of range for superheated refrigerant pressure: values capped **");
                    ShowContinueError(state, fmt::format(" Called From:{}", CalledFrom));
                    ShowContinueErrorTimeStamp(state, "");
                }
                if (CurTempRangeErrCount > 0) {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   std::string{RoutineName} + "Refrigerant [" +
                                                       state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                                       "] Temperature is out of range for superheated refrigerant pressure: values capped **",
                                                   state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupPressureTempErrIndex,
                                                   Temperature,
                                                   Temperature,
                                                   _,
                                                   "{C}",
                                                   "{C}");
                }

                // send enthalpy range error if flagged
                state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupPressureEnthErrCount += CurEnthalpyRangeErrCount;
                if (CurEnthalpyRangeErrCount > 0 && state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupPressureEnthErrCount <=
                                                        state.dataFluidProps->RefrigerantErrorLimitTest) {
                    ShowWarningMessage(state,
                                       std::string{RoutineName} + "Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                           "] Pressure is out of range for superheated refrigerant enthalpy: values capped **");
                    ShowContinueError(state, fmt::format(" Called From:{}", CalledFrom));
                    ShowContinueErrorTimeStamp(state, "");
                }
                if (CurEnthalpyRangeErrCount > 0) {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   std::string{RoutineName} + "Refrigerant [" +
                                                       state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                                       "] Pressure is out of range for superheated refrigerant pressure: values capped **",
                                                   state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupPressureEnthErrIndex,
                                                   Enthalpy,
                                                   Enthalpy,
                                                   _,
                                                   "{J}",
                                                   "{J}");
                }
            } // end error checking
        }

        return ReturnValue;
    }

    //*****************************************************************************

    Real64 GetSupHeatTempRefrig(EnergyPlusData &state,
                                std::string_view const Refrigerant, // carries in substance name
                                Real64 const Pressure,              // actual pressure given as input
                                Real64 const Enthalpy,              // actual enthalpy given as input
                                Real64 TempLow,                     // lower bound of temperature in the iteration
                                Real64 TempUp,                      // upper bound of temperature in the iteration
                                int &RefrigIndex,                   // Index to Refrigerant Properties
                                std::string_view const CalledFrom   // routine this function was called from (error messages)
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rongpeng Zhang
        //       DATE WRITTEN   Jan 2016
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Performs iterations to calculate the refrigerant temperature corresponding to the given
        // enthalpy and pressure.  Works only in superheated region.

        // METHODOLOGY EMPLOYED:
        // Perform iterations to identify the temperature by calling GetSupHeatEnthalpyRefrig.

        // USE STATEMENTS:
        using General::SolveRoot;

        // Return value
        Real64 ReturnValue;

        // FUNCTION PARAMETERS:
        // the enthalpy calculated from the pressure found
        static constexpr std::string_view RoutineName("GetSupHeatTempRefrig: ");
        static constexpr std::string_view RoutineNameNoSpace("GetSupHeatTempRefrig:");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RefrigNum;       // index for refrigerant under consideration
        Real64 EnthalpyHigh; // Enthalpy value at interpolated pressure and high temperature
        Real64 EnthalpyLow;  // Enthalpy value at interpolated pressure and low temperature
        Real64 RefTHigh;     // High Temperature Value for Ps (max in tables)
        Real64 RefTSat;      // Saturated temperature of the refrigerant. Used to check whether the refrigernat is in the superheat area
        Real64 Temp;         // Temperature of the superheated refrigerant at the given enthalpy and pressure

        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetSupHeatPressureRefrig", "properties", CalledFrom);
        }

        // Find which refrigerant (index) is being requested and then determine
        // where the temperature is within the temperature array
        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(state,
                                             state.dataFluidProps->NumOfRefrigerants,
                                             RefrigNum,
                                             true,
                                             Refrigerant,
                                             "GetSupHeatPressureRefrig",
                                             "properties",
                                             CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        // check temperature data range and attempt to cap if necessary
        RefTHigh = refrig.PsHighTempValue;
        RefTSat = GetSatTemperatureRefrig(state, Refrigerant, Pressure, RefrigNum, fmt::format("{}{}", RoutineNameNoSpace, CalledFrom));

        if (TempLow < RefTSat) {
            ShowWarningMessage(state,
                               std::string{RoutineName} + "Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                   "] temperature lower bound is out of range for superheated refrigerant: values capped **");
            ShowContinueError(state, fmt::format(" Called From:{}", CalledFrom));
            ShowContinueErrorTimeStamp(state, "");
            TempLow = RefTSat;
        }
        if (TempUp > RefTHigh) {
            ShowWarningMessage(state,
                               std::string{RoutineName} + "Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                   "] temperature lower bound is out of range for superheated refrigerant: values capped **");
            ShowContinueError(state, fmt::format(" Called From:{}", CalledFrom));
            ShowContinueErrorTimeStamp(state, "");
            TempUp = RefTHigh;
        }
        if (TempLow >= TempUp) {
            ShowWarningMessage(state,
                               std::string{RoutineName} + "Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                   "] temperature lower bound is out of range for superheated refrigerant: values capped **");
            ShowContinueError(state, fmt::format(" Called From:{}", CalledFrom));
            ShowContinueErrorTimeStamp(state, "");
            TempLow = RefTSat;
            TempUp = RefTHigh;
        }

        // check enthalpy data range and attempt to cap if necessary
        EnthalpyLow = GetSupHeatEnthalpyRefrig(state, Refrigerant, TempLow, Pressure, RefrigNum, fmt::format("{}{}", RoutineNameNoSpace, CalledFrom));
        EnthalpyHigh = GetSupHeatEnthalpyRefrig(state, Refrigerant, TempUp, Pressure, RefrigNum, fmt::format("{}{}", RoutineNameNoSpace, CalledFrom));
        if (Enthalpy <= EnthalpyLow) {
            ReturnValue = TempLow;
            return ReturnValue;
        }
        if (Enthalpy >= EnthalpyHigh) {
            ReturnValue = TempUp;
            return ReturnValue;
        }

        // Perform iterations to obtain the temperature level
        {
            std::array<Real64, 3> Par;    // Parameters passed to RegulaFalsi
            Real64 const ErrorTol(0.001); // tolerance for RegulaFalsi iterations
            int const MaxIte(500);        // maximum number of iterations
            int SolFla;                   // Flag of RegulaFalsi solver

            Par[0] = RefrigNum;
            Par[1] = Enthalpy;
            Par[2] = Pressure;

            General::SolveRoot(state, ErrorTol, MaxIte, SolFla, Temp, GetSupHeatTempRefrigResidual, TempLow, TempUp, Par);
            ReturnValue = Temp;
        }

        return ReturnValue;
    }

    Real64 GetSupHeatTempRefrigResidual(EnergyPlusData &state,
                                        Real64 const Temp, // temperature of the refrigerant
                                        std::array<Real64, 3> const &Par)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Rongpeng Zhang, LBNL
        //       DATE WRITTEN   July 2016
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (( Enthalpy_Actual - Enthalpy_Req ) / Enthalpy_Req )
        //  This method is designed to support , which calculates the refrigerant temperature corresponding to the given
        //  enthalpy and pressure in superheated region.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 TempResidual;

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // Par( 1 ) = RefrigNum;
        // Par( 2 ) = Enthalpy;
        // Par( 3 ) = Pressure;

        // FUNCTION PARAMETER DEFINITIONS:
        //  na

        // INTERFACE BLOCK SPECIFICATIONS
        //  na

        // DERIVED TYPE DEFINITIONS
        //  na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        static constexpr std::string_view RoutineNameNoSpace("GetSupHeatTempRefrigResidual");
        std::string Refrigerant; // carries in substance name
        int RefrigNum;           // index for refrigerant under consideration
        Real64 Pressure;         // pressure of the refrigerant
        Real64 Enthalpy_Req;     // enthalpy of the refrigerant to meet
        Real64 Enthalpy_Act;     // enthalpy of the refrigerant calculated

        RefrigNum = int(Par[0]);
        Enthalpy_Req = Par[1];
        Pressure = Par[2];
        Refrigerant = state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name;
        if (std::abs(Enthalpy_Req) < 100.0) Enthalpy_Req = sign(100.0, Enthalpy_Req);

        Enthalpy_Act = GetSupHeatEnthalpyRefrig(state, Refrigerant, Temp, Pressure, RefrigNum, RoutineNameNoSpace);

        TempResidual = (Enthalpy_Act - Enthalpy_Req) / Enthalpy_Req;

        return TempResidual;
    }

    //*****************************************************************************

    Real64 GetSupHeatDensityRefrig(EnergyPlusData &state,
                                   std::string_view const Refrigerant, // carries in substance name
                                   Real64 const Temperature,           // actual temperature given as input
                                   Real64 const Pressure,              // actual pressure given as input
                                   int &RefrigIndex,                   // Index to Refrigerant Properties
                                   std::string_view const CalledFrom   // routine this function was called from (error messages)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Mike Turner
        //       DATE WRITTEN   10 December 99
        //       MODIFIED       Rick Strand (April 2000, May 2000)
        //       MODIFIED       Simon Rees (May 2002)
        //       RE-ENGINEERED  N/A

        // PURPOSE OF THIS SUBROUTINE:
        // Performs linear interpolation between pressures and temperatures and
        // returns Density values.  Works only in superheated region.

        // METHODOLOGY EMPLOYED:
        // Double linear interpolation is used with Density values at four
        // pressure/temperature input points surrounding the given temperature
        // and pressure arguments.
        // With Density data it is assumed that zero values in the data are in
        // the saturated region. Hence, values near the saturation line are
        // approximated using the saturation value instead of the zero data value.
        // points completely in the saturation region are given the saturation value
        // at the given temperature. Points at the upper limits of pressure/temperature
        // have the pressure/temperature capped. Warnings are given if the point
        // is not clearly in the bounds of the superheated data.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETERS:
        static constexpr std::string_view RoutineName("GetSupHeatDensityRefrig");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TempInterpRatio;  // Interpolation ratio w.r.t temperature
        Real64 PressInterpRatio; // Interpolation ratio w.r.t pressures
        Real64 DensityHigh;      // Density value at interpolated pressure and high temperature
        Real64 DensityLow;       // Density value at interpolated pressure and low temperature
        Real64 LoTempLoDensity;  // Density value at low pressure and low temperature
        Real64 LoTempHiDensity;  // Density value at high pressure and low temperature
        Real64 HiTempLoDensity;  // Density value at low pressure and high temperature
        Real64 HiTempHiDensity;  // Density value at high pressure and high temperature

        int HiTempIndex;  // high temperature index value
        int HiPressIndex; // high pressure index value
        int LoPressIndex; // low index value of Pressure from table
        int RefrigNum;    // index for refrigerant under consideration
        int TempIndex;    // low index value of Temperature from table
        // error counters and dummy string
        int ErrCount;             // error counter for current call
        int CurTempRangeErrCount; // error counter for current call
        int CurPresRangeErrCount; // error counter for current call

        // see if data is there
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, RoutineName, "properties", CalledFrom);
        }

        ErrCount = 0; // initialize for this call
        CurTempRangeErrCount = 0;
        CurPresRangeErrCount = 0;

        // Find which refrigerant (index) is being requested and then determine
        // where the temperature and pressure are within the temperature and
        // pressure arrays, respectively
        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(
                    state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, RoutineName, "properties", CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        // check temperature data range and attempt to cap if necessary
        TempIndex = FindArrayIndex(Temperature, refrig.SHTemps, 1, refrig.NumSuperTempPts);
        if ((TempIndex > 0) && (TempIndex < refrig.NumSuperTempPts)) { // in range
            HiTempIndex = TempIndex + 1;
            TempInterpRatio = (Temperature - refrig.SHTemps(TempIndex)) / (refrig.SHTemps(HiTempIndex) - refrig.SHTemps(TempIndex));
        } else if (TempIndex < 1) {
            ++CurTempRangeErrCount;
            ++ErrCount;
            // FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
            TempIndex = 1;
            HiTempIndex = TempIndex;
            TempInterpRatio = 0.0;
        } else { // out of range
            ++CurTempRangeErrCount;
            ++ErrCount;
            // FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
            HiTempIndex = TempIndex;
            TempInterpRatio = 0.0;
        }

        // check pressure data range and attempt to cap if necessary
        LoPressIndex = FindArrayIndex(Pressure, refrig.SHPress, 1, refrig.NumSuperPressPts);
        if ((LoPressIndex > 0) && (LoPressIndex < refrig.NumSuperPressPts)) { // in range
            HiPressIndex = LoPressIndex + 1;
            Real64 const SHPress_Lo(refrig.SHPress(LoPressIndex));
            PressInterpRatio = (Pressure - SHPress_Lo) / (refrig.SHPress(HiPressIndex) - SHPress_Lo);
        } else if (LoPressIndex < 1) {
            ++CurPresRangeErrCount;
            ++ErrCount;
            LoPressIndex = 1;
            HiPressIndex = LoPressIndex;
            PressInterpRatio = 0.0;
        } else { // out of range
            ++CurPresRangeErrCount;
            ++ErrCount;
            // FindArrayIndex will return upper or lower bound so LoPressIndex gives upper/lower limit
            HiPressIndex = LoPressIndex;
            PressInterpRatio = 0.0;
        }

        // get interpolation point values
        auto const &Rhosh(refrig.RhoshValues);
        LoTempLoDensity = Rhosh(LoPressIndex, TempIndex);
        LoTempHiDensity = Rhosh(HiPressIndex, TempIndex);
        HiTempLoDensity = Rhosh(LoPressIndex, HiTempIndex);
        HiTempHiDensity = Rhosh(HiPressIndex, HiTempIndex);

        // to give reasonable interpolation near saturation reset any point with zero value
        // in table to saturation value
        int n_zero(0);
        Real64 saturated_density(0.0);
        if (min(LoTempLoDensity, LoTempHiDensity, HiTempLoDensity, HiTempHiDensity) <= 0.0) {
            saturated_density = GetSatDensityRefrig(state, Refrigerant, Temperature, 1.0, RefrigNum, RoutineName);
            if (LoTempLoDensity <= 0.0) {
                LoTempLoDensity = saturated_density;
                ++n_zero;
            }
            if (LoTempHiDensity <= 0.0) {
                LoTempHiDensity = saturated_density;
                ++n_zero;
            }
            if (HiTempLoDensity <= 0.0) {
                HiTempLoDensity = saturated_density;
                ++n_zero;
            }
            if (HiTempHiDensity <= 0.0) {
                HiTempHiDensity = saturated_density;
                ++n_zero;
            }
        }

        if (n_zero < 4) {
            // interpolate w.r.t. pressure
            DensityLow = PressInterpRatio * LoTempHiDensity + (1.0 - PressInterpRatio) * LoTempLoDensity;
            DensityHigh = PressInterpRatio * HiTempHiDensity + (1.0 - PressInterpRatio) * HiTempLoDensity;

            // interpolate w.r.t. temperature
            ReturnValue = TempInterpRatio * DensityHigh + (1.0 - TempInterpRatio) * DensityLow;
        } else { // All data is at zero: we are completely inside the saturation dome. Best thing we can do is return saturation value
            ++state.dataFluidProps->SatErrCountGetSupHeatDensityRefrig;
            // send warning
            state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupDensityErrCount += state.dataFluidProps->SatErrCountGetSupHeatDensityRefrig;
            // send warning
            if (state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupDensityErrCount <= state.dataFluidProps->RefrigerantErrorLimitTest) {
                ShowWarningMessage(state,
                                   std::string{RoutineName} + ": Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                       "] is saturated at the given conditions, saturated density at given temperature returned. **");
                ShowContinueError(state, fmt::format("...Called From:{}", CalledFrom));
                ShowContinueError(state, format("Refrigerant temperature = {:.2R}", Temperature));
                ShowContinueError(state, format("Refrigerant pressure = {:.0R}", Pressure));
                ShowContinueError(state, format("Returned Density value = {:.3R}", saturated_density));
                ShowContinueErrorTimeStamp(state, "");
            }
            if (state.dataFluidProps->SatErrCountGetSupHeatDensityRefrig > 0) {
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + ": Refrigerant [" +
                                                   state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                                   "] saturated at the given conditions **",
                                               state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupEnthalpyErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }
            return saturated_density;
        }

        if (!state.dataGlobal->WarmupFlag) {
            // some checks...
            if (ErrCount > 0) {
                // send temp range error if flagged
                state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupDensityTempErrCount += CurTempRangeErrCount;
                if (CurTempRangeErrCount > 0 && state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupDensityTempErrCount <=
                                                    state.dataFluidProps->RefrigerantErrorLimitTest) {
                    ShowWarningMessage(state,
                                       std::string{RoutineName} + ": Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                           "] Temperature is out of range for superheated refrigerant density: values capped **");
                    ShowContinueError(state, fmt::format(" Called From:{}", CalledFrom));
                    ShowContinueErrorTimeStamp(state, "");
                }
                if (CurTempRangeErrCount > 0) {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   std::string{RoutineName} + ": Refrigerant [" +
                                                       state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                                       "] Temperature is out of range for superheated refrigerant density: values capped **",
                                                   state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupDensityTempErrIndex,
                                                   Temperature,
                                                   Temperature,
                                                   _,
                                                   "{C}",
                                                   "{C}");
                }

                // send pressure range error if flagged
                state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupDensityPresErrCount += CurPresRangeErrCount;
                if (CurPresRangeErrCount > 0 && state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupDensityPresErrCount <=
                                                    state.dataFluidProps->RefrigerantErrorLimitTest) {
                    ShowWarningMessage(state,
                                       std::string{RoutineName} + ": Refrigerant [" + state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                           "] Pressure is out of range for superheated refrigerant density: values capped **");
                    ShowContinueError(state, fmt::format(" Called From:{}", CalledFrom));
                    ShowContinueErrorTimeStamp(state, "");
                }
                if (CurPresRangeErrCount > 0) {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   std::string{RoutineName} + ": Refrigerant [" +
                                                       state.dataFluidProps->RefrigErrorTracking(RefrigNum).Name +
                                                       "] Pressure is out of range for superheated refrigerant density: values capped **",
                                                   state.dataFluidProps->RefrigErrorTracking(RefrigNum).SatSupDensityPresErrIndex,
                                                   Pressure,
                                                   Pressure,
                                                   _,
                                                   "{Pa}",
                                                   "{Pa}");
                }
            } // end error checking
        }

        return ReturnValue;
    }

    //*****************************************************************************
#ifdef EP_cache_GlycolSpecificHeat
    Real64 GetSpecificHeatGlycol_raw(EnergyPlusData &state,
                                     std::string_view const Glycol,    // carries in substance name
                                     Real64 const Temperature,         // actual temperature given as input
                                     int &GlycolIndex,                 // Index to Glycol Properties
                                     std::string_view const CalledFrom // routine this function was called from (error messages)
    )
#else
    Real64 GetSpecificHeatGlycol(EnergyPlusData &state,
                                 std::string_view const Glycol,    // carries in substance name
                                 Real64 const Temperature,         // actual temperature given as input
                                 int &GlycolIndex,                 // Index to Glycol Properties
                                 std::string_view const CalledFrom // routine this function was called from (error messages)
    )
#endif
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   June 2004
        //       MODIFIED       N/A
        //       RE-ENGINEERED  N/A

        // PURPOSE OF THIS FUNCTION:
        // This subroutine finds specific heats for glycols at different
        // temperatures.

        // METHODOLOGY EMPLOYED:
        // Linear interpolation is used to find specific heat values for a
        // particular glycol (water or some mixture of water and another fluid).
        // Warnings are given if the point is not clearly in the bounds of the
        // glycol data.  The value returned is the appropriate limit value.

        // REFERENCES:
        // GetFluidPropertiesData: subroutine enforces that temperatures in
        // all temperature lists are entered in ascending order.

        // USE STATEMENTS:
        // na

        // Return value

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETERS:
        static constexpr std::string_view RoutineName("GetSpecificHeatGlycol: ");

        // Get the input if we haven't already
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        // If no glycols, no fluid properties can be evaluated
        int GlycolNum(0);
        if (state.dataFluidProps->NumOfGlycols == 0)
            ReportFatalGlycolErrors(
                state, state.dataFluidProps->NumOfGlycols, GlycolNum, true, Glycol, "GetSpecificHeatGlycol", "specific heat", CalledFrom);

        // If glycol index has not yet been found for this fluid, find its value now
        if (GlycolIndex > 0) {
            GlycolNum = GlycolIndex;
        } else { // Find which glycol (index) is being requested
            GlycolNum = FindGlycol(state, Glycol);
            if (GlycolNum == 0) {
                ReportFatalGlycolErrors(
                    state, state.dataFluidProps->NumOfGlycols, GlycolNum, true, Glycol, "GetSpecificHeatGlycol", "specific heat", CalledFrom);
            }
            GlycolIndex = GlycolNum;
        }
        auto const &glycol_data(state.dataFluidProps->GlycolData(GlycolIndex));

        // If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
        if (!glycol_data.CpDataPresent) {
            ReportFatalGlycolErrors(state,
                                    state.dataFluidProps->NumOfGlycols,
                                    GlycolNum,
                                    glycol_data.CpDataPresent,
                                    Glycol,
                                    "GetSpecificHeatGlycol",
                                    "specific heat",
                                    CalledFrom);
        }

        // Now determine the value of specific heat using interpolation
        if (Temperature < glycol_data.CpLowTempValue) { // Temperature too low
            if (!state.dataGlobal->WarmupFlag) {
                state.dataFluidProps->LowTempLimitErrGetSpecificHeatGlycol_raw =
                    ++state.dataFluidProps->GlycolErrorTracking(GlycolIndex).SpecHeatLowErrCount;
                if (state.dataFluidProps->LowTempLimitErrGetSpecificHeatGlycol_raw <= state.dataFluidProps->GlycolErrorLimitTest) {
                    ShowWarningMessage(state,
                                       std::string{RoutineName} + "Temperature is out of range (too low) for fluid [" + glycol_data.Name +
                                           "] specific heat supplied values **");
                    ShowContinueError(state,
                                      format("..Called From:{},Temperature=[{:.2R}], supplied data range=[{:.2R},{:.2R}]",
                                             CalledFrom,
                                             Temperature,
                                             glycol_data.CpLowTempValue,
                                             glycol_data.CpHighTempValue));
                    ShowContinueErrorTimeStamp(state, "");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + "Temperature out of range (too low) for fluid [" + glycol_data.Name +
                                                   "] specific heat **",
                                               state.dataFluidProps->GlycolErrorTracking(GlycolIndex).SpecHeatLowErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }
            return glycol_data.CpValues(glycol_data.CpLowTempIndex);
        } else if (Temperature > glycol_data.CpHighTempValue) { // Temperature too high
            if (!state.dataGlobal->WarmupFlag) {
                state.dataFluidProps->HighTempLimitErrGetSpecificHeatGlycol_raw =
                    ++state.dataFluidProps->GlycolErrorTracking(GlycolIndex).SpecHeatHighErrCount;
                if (state.dataFluidProps->HighTempLimitErrGetSpecificHeatGlycol_raw <= state.dataFluidProps->GlycolErrorLimitTest) {
                    ShowWarningMessage(state,
                                       std::string{RoutineName} + "Temperature is out of range (too high) for fluid [" + glycol_data.Name +
                                           "] specific heat **");
                    ShowContinueError(state,
                                      format("..Called From:{},Temperature=[{:.2R}], supplied data range=[{:.2R},{:.2R}]",
                                             CalledFrom,
                                             Temperature,
                                             glycol_data.CpLowTempValue,
                                             glycol_data.CpHighTempValue));
                    ShowContinueErrorTimeStamp(state, "");
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + "Temperature out of range (too high) for fluid [" + glycol_data.Name +
                                                   "] specific heat **",
                                               state.dataFluidProps->GlycolErrorTracking(GlycolIndex).SpecHeatHighErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }
            return glycol_data.CpValues(glycol_data.CpHighTempIndex);
        } else { // Temperature somewhere between the lowest and highest value
            auto const &glycol_CpTemps(glycol_data.CpTemps);
            auto const &glycol_CpValues(glycol_data.CpValues);
            // bracket is temp > low, <= high (for interpolation
            // for ( int Loop = glycol_data.CpLowTempIndex + 1; Loop <= glycol_data.CpHighTempIndex; ++Loop ) { //Tuned Replaced by binary search
            // below     if ( Temperature > glycol_data.CpTemps( Loop ) ) continue;     return GetInterpValue( Temperature, glycol_CpTemps( Loop - 1
            //), glycol_CpTemps( Loop ), glycol_CpValues( Loop - 1 ), glycol_CpValues( Loop ) );     break; // DO loop
            //}
            // assert( std::is_sorted( glycol_CpTemps.begin(), glycol_CpTemps.end() ) ); // Sorted temperature array is assumed: Enable if/when arrays
            // have begin()/end()
            assert(glycol_CpTemps.size() <=
                   static_cast<std::size_t>(std::numeric_limits<int>::max())); // Array indexes are int now so this is future protection
            int beg(1), mid, end(glycol_CpTemps.isize());                      // 1-based indexing
            assert(end > 0);
            while (beg + 1 < end) {
                mid = ((beg + end) >> 1); // bit shifting is faster than /2
                (Temperature > glycol_CpTemps(mid) ? beg : end) = mid;
            } // Invariant: glycol_CpTemps[beg] <= Temperature <= glycol_CpTemps[end]
            return GetInterpValue_fast(Temperature, glycol_CpTemps(beg), glycol_CpTemps(end), glycol_CpValues(beg), glycol_CpValues(end));
        }
    }

    //*****************************************************************************

    Real64 GetDensityGlycol(EnergyPlusData &state,
                            std::string_view const Glycol,    // carries in substance name
                            Real64 const Temperature,         // actual temperature given as input
                            int &GlycolIndex,                 // Index to Glycol Properties
                            std::string_view const CalledFrom // routine this function was called from (error messages)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   June 2004
        //       MODIFIED       N/A
        //       RE-ENGINEERED  N/A

        // PURPOSE OF THIS FUNCTION:
        // This subroutine finds the density for glycols at different
        // temperatures.

        // METHODOLOGY EMPLOYED:
        // Linear interpolation is used to find density values for a
        // particular glycol (water or some mixture of water and another fluid).
        // Warnings are given if the point is not clearly in the bounds of the
        // glycol data.  The value returned is the appropriate limit value.

        // REFERENCES:
        // GetFluidPropertiesData: subroutine enforces that temperatures in
        // all temperature lists are entered in ascending order.

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETERS:
        static constexpr std::string_view RoutineName("GetDensityGlycol: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Loop; // DO loop counter
        int GlycolNum;
        bool LowErrorThisTime;
        bool HighErrorThisTime;

        LowErrorThisTime = false;
        HighErrorThisTime = false;

        // Get the input if we haven't already
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        // If no glycols, no fluid properties can be evaluated
        GlycolNum = 0;
        if (state.dataFluidProps->NumOfGlycols == 0)
            ReportFatalGlycolErrors(state, state.dataFluidProps->NumOfGlycols, GlycolNum, true, Glycol, "GetDensityGlycol", "density", CalledFrom);

        // If glycol index has not yet been found for this fluid, find its value now
        if (GlycolIndex > 0) {
            GlycolNum = GlycolIndex;
        } else { // Find which refrigerant (index) is being requested
            GlycolNum = FindGlycol(state, Glycol);
            if (GlycolNum == 0) {
                ReportFatalGlycolErrors(
                    state, state.dataFluidProps->NumOfGlycols, GlycolNum, true, Glycol, "GetDensityGlycol", "density", CalledFrom);
            }
            GlycolIndex = GlycolNum;
        }

        // If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
        if (!state.dataFluidProps->GlycolData(GlycolIndex).RhoDataPresent) {
            ReportFatalGlycolErrors(state,
                                    state.dataFluidProps->NumOfGlycols,
                                    GlycolNum,
                                    state.dataFluidProps->GlycolData(GlycolIndex).RhoDataPresent,
                                    Glycol,
                                    "GetDensityGlycol",
                                    "density",
                                    CalledFrom);
        }

        // Now determine the value of specific heat using interpolation
        if (Temperature < state.dataFluidProps->GlycolData(GlycolIndex).RhoLowTempValue) { // Temperature too low
            LowErrorThisTime = true;
            ReturnValue = state.dataFluidProps->GlycolData(GlycolIndex).RhoValues(state.dataFluidProps->GlycolData(GlycolIndex).RhoLowTempIndex);
        } else if (Temperature > state.dataFluidProps->GlycolData(GlycolIndex).RhoHighTempValue) { // Temperature too high
            HighErrorThisTime = true;
            ReturnValue = state.dataFluidProps->GlycolData(GlycolIndex).RhoValues(state.dataFluidProps->GlycolData(GlycolIndex).RhoHighTempIndex);
        } else { // Temperature somewhere between the lowest and highest value
            ReturnValue = state.dataFluidProps->GlycolData(GlycolIndex).RhoValues(state.dataFluidProps->GlycolData(GlycolIndex).RhoLowTempIndex);
            // bracket is temp > low, <= high (for interpolation
            for (Loop = state.dataFluidProps->GlycolData(GlycolIndex).RhoLowTempIndex + 1;
                 Loop <= state.dataFluidProps->GlycolData(GlycolIndex).RhoHighTempIndex;
                 ++Loop) {
                if (Temperature > state.dataFluidProps->GlycolData(GlycolIndex).RhoTemps(Loop)) continue;
                ReturnValue = GetInterpValue(state,
                                             Temperature,
                                             state.dataFluidProps->GlycolData(GlycolIndex).RhoTemps(Loop - 1),
                                             state.dataFluidProps->GlycolData(GlycolIndex).RhoTemps(Loop),
                                             state.dataFluidProps->GlycolData(GlycolIndex).RhoValues(Loop - 1),
                                             state.dataFluidProps->GlycolData(GlycolIndex).RhoValues(Loop));
                break; // DO loop
            }
        }

        // Error handling
        if (!state.dataGlobal->WarmupFlag) {

            if (LowErrorThisTime) {
                ++state.dataFluidProps->GlycolErrorTracking(GlycolIndex).DensityLowErrCount;
                state.dataFluidProps->LowTempLimitErrGetDensityGlycol = state.dataFluidProps->GlycolErrorTracking(GlycolIndex).DensityLowErrCount;
            }
            if (HighErrorThisTime) {
                ++state.dataFluidProps->GlycolErrorTracking(GlycolIndex).DensityHighErrCount;
                state.dataFluidProps->HighTempLimitErrGetDensityGlycol = state.dataFluidProps->GlycolErrorTracking(GlycolIndex).DensityHighErrCount;
            }

            if ((LowErrorThisTime) && (state.dataFluidProps->LowTempLimitErrGetDensityGlycol <= state.dataFluidProps->GlycolErrorLimitTest)) {
                ShowWarningMessage(state,
                                   std::string{RoutineName} + "Temperature is out of range (too low) for fluid [" +
                                       state.dataFluidProps->GlycolData(GlycolIndex).Name + "] density **");
                ShowContinueError(state,
                                  format("..Called From:{},Temperature=[{:.2R}], supplied data range=[{:.2R},{:.2R}]",
                                         CalledFrom,
                                         Temperature,
                                         state.dataFluidProps->GlycolData(GlycolIndex).RhoLowTempValue,
                                         state.dataFluidProps->GlycolData(GlycolIndex).RhoHighTempValue));
                ShowContinueErrorTimeStamp(state, "");
            }
            if (LowErrorThisTime) {
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + "Temperature out of range (too low) for fluid [" +
                                                   state.dataFluidProps->GlycolData(GlycolIndex).Name + "] density **",
                                               state.dataFluidProps->GlycolErrorTracking(GlycolIndex).DensityLowErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }

            if ((HighErrorThisTime) && (state.dataFluidProps->HighTempLimitErrGetDensityGlycol <= state.dataFluidProps->GlycolErrorLimitTest)) {
                ShowWarningMessage(state,
                                   std::string{RoutineName} + "Temperature is out of range (too high) for fluid [" +
                                       state.dataFluidProps->GlycolData(GlycolIndex).Name + "] density **");
                ShowContinueError(state,
                                  format("..Called From:{},Temperature=[{:.2R}], supplied data range=[{:.2R},{:.2R}]",
                                         CalledFrom,
                                         Temperature,
                                         state.dataFluidProps->GlycolData(GlycolIndex).RhoLowTempValue,
                                         state.dataFluidProps->GlycolData(GlycolIndex).RhoHighTempValue));
                ShowContinueErrorTimeStamp(state, "");
            }
            if (HighErrorThisTime) {
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + "Temperature out of range (too high) for fluid [" +
                                                   state.dataFluidProps->GlycolData(GlycolIndex).Name + "] density **",
                                               state.dataFluidProps->GlycolErrorTracking(GlycolIndex).DensityHighErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }
        }

        return ReturnValue;
    }

    //*****************************************************************************

    Real64 GetConductivityGlycol(EnergyPlusData &state,
                                 std::string_view const Glycol,    // carries in substance name
                                 Real64 const Temperature,         // actual temperature given as input
                                 int &GlycolIndex,                 // Index to Glycol Properties
                                 std::string_view const CalledFrom // routine this function was called from (error messages)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   June 2004
        //       MODIFIED       N/A
        //       RE-ENGINEERED  N/A

        // PURPOSE OF THIS FUNCTION:
        // This subroutine finds the conductivity for glycols at different
        // temperatures.

        // METHODOLOGY EMPLOYED:
        // Linear interpolation is used to find conductivity values for a
        // particular glycol (water or some mixture of water and another fluid).
        // Warnings are given if the point is not clearly in the bounds of the
        // glycol data.  The value returned is the appropriate limit value.

        // REFERENCES:
        // GetFluidPropertiesData: subroutine enforces that temperatures in
        // all temperature lists are entered in ascending order.

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETERS:
        static constexpr std::string_view RoutineName("GetConductivityGlycol: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Loop; // DO loop counter
        int GlycolNum;
        bool LowErrorThisTime;
        bool HighErrorThisTime;

        LowErrorThisTime = false;
        HighErrorThisTime = false;

        // Get the input if we haven't already
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        // If no glycols, no fluid properties can be evaluated
        GlycolNum = 0;
        if (state.dataFluidProps->NumOfGlycols == 0)
            ReportFatalGlycolErrors(
                state, state.dataFluidProps->NumOfGlycols, GlycolNum, true, Glycol, "GetConductivityGlycol", "conductivity", CalledFrom);

        // If glycol index has not yet been found for this fluid, find its value now
        if (GlycolIndex > 0) {
            GlycolNum = GlycolIndex;
        } else { // Find which refrigerant (index) is being requested
            GlycolNum = FindGlycol(state, Glycol);
            if (GlycolNum == 0) {
                ReportFatalGlycolErrors(
                    state, state.dataFluidProps->NumOfGlycols, GlycolNum, true, Glycol, "GetConductivityGlycol", "conductivity", CalledFrom);
            }
            GlycolIndex = GlycolNum;
        }

        // If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
        if (!state.dataFluidProps->GlycolData(GlycolIndex).CondDataPresent) {
            ReportFatalGlycolErrors(state,
                                    state.dataFluidProps->NumOfGlycols,
                                    GlycolNum,
                                    state.dataFluidProps->GlycolData(GlycolIndex).CondDataPresent,
                                    Glycol,
                                    "GetConductivityGlycol",
                                    "conductivity",
                                    CalledFrom);
        }

        // Now determine the value of specific heat using interpolation
        if (Temperature < state.dataFluidProps->GlycolData(GlycolIndex).CondLowTempValue) { // Temperature too low
            LowErrorThisTime = true;
            ReturnValue = state.dataFluidProps->GlycolData(GlycolIndex).CondValues(state.dataFluidProps->GlycolData(GlycolIndex).CondLowTempIndex);
        } else if (Temperature > state.dataFluidProps->GlycolData(GlycolIndex).CondHighTempValue) { // Temperature too high
            HighErrorThisTime = true;
            ReturnValue = state.dataFluidProps->GlycolData(GlycolIndex).CondValues(state.dataFluidProps->GlycolData(GlycolIndex).CondHighTempIndex);
        } else { // Temperature somewhere between the lowest and highest value
            ReturnValue = state.dataFluidProps->GlycolData(GlycolIndex).CondValues(state.dataFluidProps->GlycolData(GlycolIndex).CondLowTempIndex);
            // bracket is temp > low, <= high (for interpolation
            for (Loop = state.dataFluidProps->GlycolData(GlycolIndex).CondLowTempIndex + 1;
                 Loop <= state.dataFluidProps->GlycolData(GlycolIndex).CondHighTempIndex;
                 ++Loop) {
                if (Temperature > state.dataFluidProps->GlycolData(GlycolIndex).CondTemps(Loop)) continue;
                ReturnValue = GetInterpValue(state,
                                             Temperature,
                                             state.dataFluidProps->GlycolData(GlycolIndex).CondTemps(Loop - 1),
                                             state.dataFluidProps->GlycolData(GlycolIndex).CondTemps(Loop),
                                             state.dataFluidProps->GlycolData(GlycolIndex).CondValues(Loop - 1),
                                             state.dataFluidProps->GlycolData(GlycolIndex).CondValues(Loop));
                break; // DO loop
            }
        }

        // Error handling
        if (!state.dataGlobal->WarmupFlag) {

            if (LowErrorThisTime) {
                ++state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ConductivityLowErrCount;
                state.dataFluidProps->LowTempLimitErrGetConductivityGlycol =
                    state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ConductivityLowErrCount;
            }
            if (HighErrorThisTime) {
                ++state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ConductivityHighErrCount;
                state.dataFluidProps->HighTempLimitErrGetConductivityGlycol =
                    state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ConductivityHighErrCount;
            }

            if ((LowErrorThisTime) && (state.dataFluidProps->LowTempLimitErrGetConductivityGlycol <= state.dataFluidProps->GlycolErrorLimitTest)) {
                ShowWarningMessage(state,
                                   std::string{RoutineName} + "Temperature is out of range (too low) for fluid [" +
                                       state.dataFluidProps->GlycolData(GlycolIndex).Name + "] conductivity **");
                ShowContinueError(state,
                                  format("..Called From:{},Temperature=[{:.2R}], supplied data range=[{:.2R},{:.2R}]",
                                         CalledFrom,
                                         Temperature,
                                         state.dataFluidProps->GlycolData(GlycolIndex).CondLowTempValue,
                                         state.dataFluidProps->GlycolData(GlycolIndex).CondHighTempValue));
                ShowContinueErrorTimeStamp(state, "");
            }
            if (LowErrorThisTime) {
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + "Temperature out of range (too low) for fluid [" +
                                                   state.dataFluidProps->GlycolData(GlycolIndex).Name + "] conductivity **",
                                               state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ConductivityLowErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }

            if ((HighErrorThisTime) && (state.dataFluidProps->HighTempLimitErrGetConductivityGlycol <= state.dataFluidProps->GlycolErrorLimitTest)) {
                ShowWarningMessage(state,
                                   std::string{RoutineName} + "Temperature is out of range (too high) for fluid [" +
                                       state.dataFluidProps->GlycolData(GlycolIndex).Name + "] conductivity **");
                ShowContinueError(state,
                                  format("..Called From:{},Temperature=[{:.2R}], supplied data range=[{:.2R},{:.2R}]",
                                         CalledFrom,
                                         Temperature,
                                         state.dataFluidProps->GlycolData(GlycolIndex).CondLowTempValue,
                                         state.dataFluidProps->GlycolData(GlycolIndex).CondHighTempValue));
                ShowContinueErrorTimeStamp(state, "");
            }
            if (HighErrorThisTime) {
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + "Temperature out of range (too high) for fluid [" +
                                                   state.dataFluidProps->GlycolData(GlycolIndex).Name + "] conductivity **",
                                               state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ConductivityHighErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }
        }

        return ReturnValue;
    }

    //*****************************************************************************

    Real64 GetViscosityGlycol(EnergyPlusData &state,
                              std::string_view const Glycol,    // carries in substance name
                              Real64 const Temperature,         // actual temperature given as input
                              int &GlycolIndex,                 // Index to Glycol Properties
                              std::string_view const CalledFrom // routine this function was called from (error messages)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   June 2004
        //       MODIFIED       N/A
        //       RE-ENGINEERED  N/A

        // PURPOSE OF THIS FUNCTION:
        // This subroutine finds the viscosity for glycols at different
        // temperatures.

        // METHODOLOGY EMPLOYED:
        // Linear interpolation is used to find viscosity values for a
        // particular glycol (water or some mixture of water and another fluid).
        // Warnings are given if the point is not clearly in the bounds of the
        // glycol data.  The value returned is the appropriate limit value.

        // REFERENCES:
        // GetFluidPropertiesData: subroutine enforces that temperatures in
        // all temperature lists are entered in ascending order.

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue; // Value for function

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETERS:
        static constexpr std::string_view RoutineName("GetViscosityGlycol: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Loop; // DO loop counter
        int GlycolNum;
        bool LowErrorThisTime;
        bool HighErrorThisTime;

        LowErrorThisTime = false;
        HighErrorThisTime = false;

        // Get the input if we haven't already
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        // If no glycols, no fluid properties can be evaluated
        GlycolNum = 0;
        if (state.dataFluidProps->NumOfGlycols == 0)
            ReportFatalGlycolErrors(
                state, state.dataFluidProps->NumOfGlycols, GlycolNum, true, Glycol, "GetViscosityGlycol", "viscosity", CalledFrom);

        // If glycol index has not yet been found for this fluid, find its value now
        if (GlycolIndex > 0) {
            GlycolNum = GlycolIndex;
        } else { // Find which refrigerant (index) is being requested
            GlycolNum = FindGlycol(state, Glycol);
            if (GlycolNum == 0) {
                ReportFatalGlycolErrors(
                    state, state.dataFluidProps->NumOfGlycols, GlycolNum, true, Glycol, "GetViscosityGlycol", "viscosity", CalledFrom);
            }
            GlycolIndex = GlycolNum;
        }

        // If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
        if (!state.dataFluidProps->GlycolData(GlycolIndex).ViscDataPresent) {
            ReportFatalGlycolErrors(state,
                                    state.dataFluidProps->NumOfGlycols,
                                    GlycolNum,
                                    state.dataFluidProps->GlycolData(GlycolIndex).ViscDataPresent,
                                    Glycol,
                                    "GetViscosityGlycol",
                                    "viscosity",
                                    CalledFrom);
        }

        // Now determine the value of specific heat using interpolation
        if (Temperature < state.dataFluidProps->GlycolData(GlycolIndex).ViscLowTempValue) { // Temperature too low
            LowErrorThisTime = true;
            ReturnValue = state.dataFluidProps->GlycolData(GlycolIndex).ViscValues(state.dataFluidProps->GlycolData(GlycolIndex).ViscLowTempIndex);
        } else if (Temperature > state.dataFluidProps->GlycolData(GlycolIndex).ViscHighTempValue) { // Temperature too high
            HighErrorThisTime = true;
            ReturnValue = state.dataFluidProps->GlycolData(GlycolIndex).ViscValues(state.dataFluidProps->GlycolData(GlycolIndex).ViscHighTempIndex);
        } else { // Temperature somewhere between the lowest and highest value
            ReturnValue = state.dataFluidProps->GlycolData(GlycolIndex).ViscValues(state.dataFluidProps->GlycolData(GlycolIndex).ViscLowTempIndex);
            // bracket is temp > low, <= high (for interpolation
            for (Loop = state.dataFluidProps->GlycolData(GlycolIndex).ViscLowTempIndex + 1;
                 Loop <= state.dataFluidProps->GlycolData(GlycolIndex).ViscHighTempIndex;
                 ++Loop) {
                if (Temperature > state.dataFluidProps->GlycolData(GlycolIndex).ViscTemps(Loop)) continue;
                ReturnValue = GetInterpValue(state,
                                             Temperature,
                                             state.dataFluidProps->GlycolData(GlycolIndex).ViscTemps(Loop - 1),
                                             state.dataFluidProps->GlycolData(GlycolIndex).ViscTemps(Loop),
                                             state.dataFluidProps->GlycolData(GlycolIndex).ViscValues(Loop - 1),
                                             state.dataFluidProps->GlycolData(GlycolIndex).ViscValues(Loop));
                break; // DO loop
            }
        }

        // Error handling
        if (!state.dataGlobal->WarmupFlag) {

            if (LowErrorThisTime) {
                ++state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ViscosityLowErrCount;
                state.dataFluidProps->LowTempLimitErrGetViscosityGlycol = state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ViscosityLowErrCount;
            }
            if (HighErrorThisTime) {
                ++state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ViscosityHighErrCount;
                state.dataFluidProps->HighTempLimitErrGetViscosityGlycol =
                    state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ViscosityHighErrCount;
            }

            if ((LowErrorThisTime) && (state.dataFluidProps->LowTempLimitErrGetViscosityGlycol <= state.dataFluidProps->GlycolErrorLimitTest)) {
                ShowWarningMessage(state,
                                   std::string{RoutineName} + "Temperature is out of range (too low) for fluid [" +
                                       state.dataFluidProps->GlycolData(GlycolIndex).Name + "] viscosity **");
                ShowContinueError(state,
                                  format("..Called From:{},Temperature=[{:.2R}], supplied data range=[{:.2R},{:.2R}]",
                                         CalledFrom,
                                         Temperature,
                                         state.dataFluidProps->GlycolData(GlycolIndex).ViscLowTempValue,
                                         state.dataFluidProps->GlycolData(GlycolIndex).ViscHighTempValue));
                ShowContinueErrorTimeStamp(state, "");
            }
            if (LowErrorThisTime) {
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + "Temperature out of range (too low) for fluid [" +
                                                   state.dataFluidProps->GlycolData(GlycolIndex).Name + "] viscosity **",
                                               state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ViscosityLowErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }

            if ((HighErrorThisTime) && (state.dataFluidProps->HighTempLimitErrGetViscosityGlycol <= state.dataFluidProps->GlycolErrorLimitTest)) {
                ShowWarningMessage(state,
                                   std::string{RoutineName} + "Temperature is out of range (too high) for fluid [" +
                                       state.dataFluidProps->GlycolData(GlycolIndex).Name + "] viscosity **");
                ShowContinueError(state,
                                  format("..Called From:{},Temperature=[{:.2R}], supplied data range=[{:.2R},{:.2R}]",
                                         CalledFrom,
                                         Temperature,
                                         state.dataFluidProps->GlycolData(GlycolIndex).ViscLowTempValue,
                                         state.dataFluidProps->GlycolData(GlycolIndex).ViscHighTempValue));
                ShowContinueErrorTimeStamp(state, "");
            }
            if (HighErrorThisTime) {
                ShowRecurringWarningErrorAtEnd(state,
                                               std::string{RoutineName} + "Temperature out of range (too high) for fluid [" +
                                                   state.dataFluidProps->GlycolData(GlycolIndex).Name + "] viscosity **",
                                               state.dataFluidProps->GlycolErrorTracking(GlycolIndex).ViscosityHighErrIndex,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }
        }

        return ReturnValue;
    }

    //*****************************************************************************

    void GetInterpValue_error(EnergyPlusData &state)
    {
        ShowFatalError(state, "GetInterpValue: Temperatures for fluid property data too close together, division by zero");
    }

    //*****************************************************************************

    Real64 GetQualityRefrig(EnergyPlusData &state,
                            std::string const &Refrigerant,   // carries in substance name
                            Real64 const Temperature,         // actual temperature given as input
                            Real64 const Enthalpy,            // actual enthalpy given as input
                            int &RefrigIndex,                 // Index to Refrigerant Properties
                            std::string_view const CalledFrom // routine this function was called from (error messages)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Simon Rees (May 2002)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function determines the quality of a refrigerant in the saturate
        // region based on its temperature and enthalpy

        // METHODOLOGY EMPLOYED:
        // Just checks to see whether or not the refrigerant name coming in can
        // be found in the refrigerant derived type.  If so, the "reverse" of the
        // GetSatEnthalpyRefrig function is performed.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 SatVapEnthalpy;  // value of enthalpy at hi index value for given Quality
        Real64 SatLiqEnthalpy;  // value of enthalpy at TempIndex index value for given Quality
        int RefrigNum;          // index for refrigerant under consideration
        int HiTempIndex;        // array index for temp above input temp
        int LoTempIndex;        // array index for temp below input temp
        Real64 TempInterpRatio; // ratio to interpolate in temperature domain

        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        RefrigNum = 0;
        if (state.dataFluidProps->NumOfRefrigerants == 0) {
            ReportFatalRefrigerantErrors(
                state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetQualityRefrig", "enthalpy", CalledFrom);
        }

        // Find which refrigerant (index) is being requested and then determine
        // where the temperature is within the temperature array
        if (RefrigIndex > 0) {
            RefrigNum = RefrigIndex;
        } else {
            // Find which refrigerant (index) is being requested
            RefrigNum = FindRefrigerant(state, Refrigerant);
            if (RefrigNum == 0) {
                ReportFatalRefrigerantErrors(
                    state, state.dataFluidProps->NumOfRefrigerants, RefrigNum, true, Refrigerant, "GetQualityRefrig", "enthalpy", CalledFrom);
            }
            RefrigIndex = RefrigNum;
        }
        auto const &refrig(state.dataFluidProps->RefrigData(RefrigNum));

        LoTempIndex = FindArrayIndex(Temperature, refrig.HTemps, refrig.HfLowTempIndex, refrig.HfHighTempIndex);
        HiTempIndex = LoTempIndex + 1;

        // check on the data bounds and adjust indices to give clamped return value
        if (LoTempIndex == 0) {
            SatLiqEnthalpy = refrig.HfValues(refrig.HfLowTempIndex);
            SatVapEnthalpy = refrig.HfgValues(refrig.HfLowTempIndex);
            // Temperature supplied is out of bounds--produce an error message...
            if (!state.dataGlobal->WarmupFlag)
                ShowRecurringWarningErrorAtEnd(state,
                                               "GetQualityRefrig: ** Temperature for requested quality is below the range of data supplied **",
                                               state.dataFluidProps->TempLoRangeErrIndexGetQualityRefrig,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");

        } else if (HiTempIndex > refrig.NumHPoints) {
            SatLiqEnthalpy = refrig.HfValues(refrig.HfHighTempIndex);
            SatVapEnthalpy = refrig.HfgValues(refrig.HfHighTempIndex);
            // Temperature supplied is out of bounds--produce an error message...
            if (!state.dataGlobal->WarmupFlag)
                ShowRecurringWarningErrorAtEnd(state,
                                               "GetQualityRefrig: ** Temperature requested quality is above the range of data supplied **",
                                               state.dataFluidProps->TempHiRangeErrIndexGetQualityRefrig,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");

        } else { // in normal range work out interpolated liq and gas enthalpies
            TempInterpRatio = (Temperature - refrig.HTemps(LoTempIndex)) / (refrig.HTemps(HiTempIndex) - refrig.HTemps(LoTempIndex));
            SatLiqEnthalpy = TempInterpRatio * refrig.HfValues(HiTempIndex) + (1.0 - TempInterpRatio) * refrig.HfValues(LoTempIndex);
            SatVapEnthalpy = TempInterpRatio * refrig.HfgValues(HiTempIndex) + (1.0 - TempInterpRatio) * refrig.HfgValues(LoTempIndex);
        }

        // calculate final quality value from enthalpy ratio
        ReturnValue = (Enthalpy - SatLiqEnthalpy) / (SatVapEnthalpy - SatLiqEnthalpy);

        // final check to bound returned quality value
        if (ReturnValue < 0.0) {
            //    CALL ShowRecurringWarningErrorAtEnd(state, 'GetQualityRefrig: ** '//  &
            //                   'Quality is less than zero in GetQualityRefrig; Quality reset to 0.0 **')
            ReturnValue = 0.0;
        } else if (ReturnValue > 1.0) {
            //    CALL ShowRecurringWarningErrorAtEnd(state, 'GetQualityRefrig: ** '//  &
            //                   'Quality is greater than one in GetQualityRefrig; refrigerant is superheated **')
            ReturnValue = 2.0;
        }

        return ReturnValue;
    }

    //*****************************************************************************

    int FindRefrigerant(EnergyPlusData &state, std::string_view const Refrigerant) // carries in substance name
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Simon Rees (June 2002)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function simply determines the index of the refrigerant named
        // in the input variable to this routine within the derived type.

        // METHODOLOGY EMPLOYED:
        // Just checks to see whether or not the refrigerant name coming in can
        // be found in the refrigerant derived type.  If so, the function is set
        // to the index within the derived type.  If the input has not been read
        // yet for some reason, that must be done.

        // Return value
        int FindRefrigerant;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Found; // Indicator for found item

        // Make sure we have already read in the input
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        // Check to see if this glycol shows up in the glycol data
        Found = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase(Refrigerant), state.dataFluidProps->RefrigData);

        if (Found > 0) {
            FindRefrigerant = Found;
            state.dataFluidProps->RefrigUsed(Found) = true;
        } else { // not found - errors handled in calling proceedure
            FindRefrigerant = 0;
        }

        return FindRefrigerant;
    }

    //*****************************************************************************

    int FindGlycol(EnergyPlusData &state, std::string_view const Glycol) // carries in substance name
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Simon Rees (June 2002)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function simply determines the index of the glycol named
        // in the input variable to this routine within the derived type.

        // METHODOLOGY EMPLOYED:
        // Just checks to see whether or not the glycol name coming in can
        // be found in the glycol derived type.  If so, the function is set
        // to the index within the derived type.  If the input has not been read
        // yet for some reason, that must be done.

        // Return value
        int FindGlycol;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Found; // Indicator for found item

        // Make sure we have already read in the input
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        // Check to see if this glycol shows up in the glycol data
        Found = UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase(Glycol),
                                                state.dataFluidProps->GlycolData,
                                                state.dataFluidProps->NumOfGlycols); // GlycolData is allocated to NumOfGlyConcs

        if (Found > 0) {
            FindGlycol = Found;
            state.dataFluidProps->GlycolUsed(Found) = true;
        } else { // return zero - error checking in calling proceedure
            FindGlycol = 0;
        }

        return FindGlycol;
    }

    //*****************************************************************************

    std::string GetGlycolNameByIndex(EnergyPlusData &state, int const Idx) // carries in substance index
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Edwin Lee
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function simply returns the glycol name by index from the
        // GlycolData data structure.  This is needed to expose the name
        // as the data structure is private.
        // This is used by plant equipment to pass in both the proper index
        // and the proper name when calling glycol routines.  Thus, the index
        // is already known, and the input is assumed to be found.

        // METHODOLOGY EMPLOYED:
        // Just checks to see whether or not the glycol index is valid
        // and if so, the function returns the name.  If not, it returns ' '

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        // na

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        // na

        // Check to see if this glycol shows up in the glycol data
        //  ArrayLength = SIZE(GlycolData)

        if (Idx > 0 && Idx <= state.dataFluidProps->NumOfGlycols) {
            return state.dataFluidProps->GlycolData(Idx).Name;
        } else { // return blank - error checking in calling proceedure
            return "";
        }
    }

    //*****************************************************************************

    int FindArrayIndex(Real64 const Value,           // Value to be placed/found within the array of values
                       Array1D<Real64> const &Array, // Array of values in ascending order
                       int const LowBound,           // Valid values lower bound (set by calling program)
                       int const UpperBound          // Valid values upper bound (set by calling program)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Simon Rees (May 2002)
        //       RE-ENGINEERED  Autodesk (Nov 2013) (performance tuned on C++)

        // PURPOSE OF THIS FUNCTION:
        // This generic function simply finds the points in an array between
        // which a single value is found.  The returned value is the index of
        // the low point.

        // METHODOLOGY EMPLOYED:
        // Straight interval halving. It is assumed that the values in the array
        // appear in ascending order. If the value is below that in the supplied
        // data array a zero index is returned. If the value is above that in the
        // supplied data array, the max index is returned. This allows some error
        // checking in the calling routine.

        // Autodesk:Tuned Profiling hot spot: Slightly slower when inlined

        // Bit shifting is substantially faster than /2 at least on GCC even with high optimization
        // Linear indexing used to assure we are bit shifting positive values where behavior is assured
        // std::lower_bound was 4x slower for the small (~100) array sizes seen in EnergyPlus use
        typedef Array1D<Real64>::size_type size_type;
        int const l(Array.l());
        assert(LowBound >= l);
        assert(LowBound <= UpperBound);
        assert(UpperBound <= Array.u());
        assert(Array.size() > 0u); // Empty arrays are not currently supported
        assert(l > 0);             // Returning 0 for Value smaller than lowest doesn't make sense if l() <= 0
        size_type beg(LowBound - l);
        if (Value < Array[beg]) {
            return 0;
        } else {
            size_type end(UpperBound - l);
            if (Value > Array[end]) {
                return UpperBound;
            } else { // Binary search
                size_type mid;
                while (beg + 1 < end) {
                    mid = ((beg + end) >> 1);
                    (Value > Array[mid] ? beg : end) = mid;
                }
                return l + beg;
            }
        }
    }

    int FindArrayIndex(Real64 const Value,          // Value to be placed/found within the array of values
                       Array1D<Real64> const &Array // Array of values in ascending order
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Simon Rees (May 2002)
        //       RE-ENGINEERED  Autodesk (Nov 2013) (performance tuned on C++)

        // PURPOSE OF THIS FUNCTION:
        // This generic function simply finds the points in an array between
        // which a single value is found.  The returned value is the index of
        // the low point.

        // METHODOLOGY EMPLOYED:
        // Straight interval halving. It is assumed that the values in the array
        // appear in ascending order. If the value is below that in the supplied
        // data array a zero index is returned. If the value is above that in the
        // supplied data array, the max index is returned. This allows some error
        // checking in the calling routine.

        // Autodesk:Tuned Profiling hot spot: Slightly slower when inlined

        // Bit shifting is substantially faster than /2 at least on GCC even with high optimization
        // Linear indexing used to assure we are bit shifting positive values where behavior is assured
        // std::lower_bound was 4x slower for the small (~100) array sizes seen in EnergyPlus use
        typedef Array1D<Real64>::size_type size_type;
        assert(Array.size() > 0u); // Empty arrays are not currently supported
        assert(Array.l() > 0);     // Returning 0 for Value smaller than lowest doesn't make sense if l() <= 0
        if (Value < Array[0]) {
            return 0;
        } else {
            size_type end(Array.size() - 1u);
            if (Value > Array[end]) {
                return Array.u();
            } else { // Binary search
                size_type beg(0), mid;
                while (beg + 1 < end) {
                    mid = ((beg + end) >> 1);
                    (Value > Array[mid] ? beg : end) = mid;
                }
                return Array.l() + beg;
            }
        }
    }

    //*****************************************************************************

    Real64 GetInterpolatedSatProp(EnergyPlusData &state,
                                  Real64 const Temperature,          // Saturation Temp.
                                  Array1D<Real64> const &PropTemps,  // Array of temperature at which props are available
                                  Array1D<Real64> const &LiqProp,    // Array of saturated liquid properties
                                  Array1D<Real64> const &VapProp,    // Array of saturatedvapour properties
                                  Real64 const Quality,              // Quality
                                  std::string_view const CalledFrom, // routine this function was called from (error messages)
                                  int const LowBound,                // Valid values lower bound (set by calling program)
                                  int const UpperBound               // Valid values upper bound (set by calling program)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Simon Rees
        //       DATE WRITTEN   May 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This generic function performs an interpolation on the supplied saturated
        // liquid and vapor data to find the saturated property value at a given
        // temperature and quality. This function is used by all the functions that
        // get saturated property values.

        // METHODOLOGY EMPLOYED:
        // Index of arrays either side of given temperature is found using FindArrayIndex.
        // Double linear interpolation is used to first find property values at the given
        // quality bounding the required temperature. These values are interpolated in the
        // temperature domain to find the final value.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 ReturnValue;

        // Argument array dimensioning

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        // error counters and dummy string
        bool ErrorFlag(false); // error flag for current call

        int const LoTempIndex = FindArrayIndex(Temperature, PropTemps, LowBound, UpperBound); // array index for temp above input temp

        if (LoTempIndex == 0) {
            ReturnValue = LiqProp(LowBound) + Quality * (VapProp(LowBound) - LiqProp(LowBound));
            ErrorFlag = true;
        } else if (LoTempIndex >= UpperBound) {
            ReturnValue = LiqProp(UpperBound) + Quality * (VapProp(UpperBound) - LiqProp(UpperBound));
            ErrorFlag = true;
        } else {
            int const HiTempIndex = LoTempIndex + 1; // array index for temp below input temp

            // find adjacent property values at the given quality
            Real64 const LiqProp_Lo = LiqProp(LoTempIndex);
            Real64 const LoSatProp = LiqProp_Lo + Quality * (VapProp(LoTempIndex) - LiqProp_Lo); // Sat. prop. at lower temp & given quality

            Real64 const LiqProp_Hi = LiqProp(HiTempIndex);
            Real64 const HiSatProp = LiqProp_Hi + Quality * (VapProp(HiTempIndex) - LiqProp_Hi); // Sat. prop. at higher temp & given quality

            // find interpolation ratio in temperature direction
            Real64 const PropTemps_Lo = PropTemps(LoTempIndex);
            Real64 const TempInterpRatio = (Temperature - PropTemps_Lo) / (PropTemps(HiTempIndex) - PropTemps_Lo);

            // apply final linear interpolation
            ReturnValue = LoSatProp + TempInterpRatio * (HiSatProp - LoSatProp);
        }

        if (ErrorFlag && (CalledFrom != "ReportAndTestRefrigerants")) {
            ++state.dataFluidProps->TempRangeErrCountGetInterpolatedSatProp;
            // send warning
            if (state.dataFluidProps->TempRangeErrCountGetInterpolatedSatProp <= state.dataFluidProps->RefrigerantErrorLimitTest) {
                ShowWarningError(state, "GetInterpolatedSatProp: Saturation temperature for interpolation is out of range of data supplied: **");
                ShowContinueErrorTimeStamp(state, fmt::format(" Called from:{}", CalledFrom));
                ShowContinueError(state, format("Refrigerant temperature = {:.2R}", Temperature));
                ShowContinueError(state, format("Returned saturated property value = {:.3R}", ReturnValue));
            } else {
                ShowRecurringWarningErrorAtEnd(state,
                                               "GetInterpolatedSatProp: Refrigerant temperature for interpolation out of range error",
                                               state.dataFluidProps->TempRangeErrIndexGetInterpolatedSatProp,
                                               Temperature,
                                               Temperature,
                                               _,
                                               "{C}",
                                               "{C}");
            }
        }

        return ReturnValue;
    }

    //*****************************************************************************

    int CheckFluidPropertyName(EnergyPlusData &state,
                               std::string const &NameToCheck) // Name from input(?) to be checked against valid FluidPropertyNames
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   October 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function checks on an input fluid property to make sure it is valid.

        // Return value
        int CheckFluidPropertyName;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Found;

        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        // Item must be either in Refrigerant or Glycol list
        Found = 0;
        if (state.dataFluidProps->NumOfRefrigerants > 0) {
            Found = UtilityRoutines::FindItemInList(NameToCheck, state.dataFluidProps->RefrigData);
        }
        if (Found == 0) {
            if (state.dataFluidProps->NumOfGlycols > 0) {
                Found = UtilityRoutines::FindItemInList(
                    NameToCheck, state.dataFluidProps->GlycolData, state.dataFluidProps->NumOfGlycols); // GlycolData is allocated to NumOfGlyConcs
            }
        }

        CheckFluidPropertyName = Found;

        return CheckFluidPropertyName;
    }

    void ReportOrphanFluids(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // In response to CR8008, report orphan (unused) fluid items.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool NeedOrphanMessage;
        int Item;
        int NumUnusedRefrig;
        int NumUnusedGlycol;

        NeedOrphanMessage = true;
        NumUnusedRefrig = 0;

        for (Item = 1; Item <= state.dataFluidProps->NumOfRefrigerants; ++Item) {
            if (state.dataFluidProps->RefrigUsed(Item)) continue;
            if (UtilityRoutines::SameString(state.dataFluidProps->RefrigData(Item).Name, Steam)) continue;
            if (NeedOrphanMessage && state.dataGlobal->DisplayUnusedObjects) {
                ShowWarningError(state, "The following fluid names are \"Unused Fluids\".  These fluids are in the idf");
                ShowContinueError(state, " file but are never obtained by the simulation and therefore are NOT used.");
                NeedOrphanMessage = false;
            }
            if (state.dataGlobal->DisplayUnusedObjects) {
                ShowMessage(state, "Refrigerant=" + state.dataFluidProps->RefrigData(Item).Name);
            } else {
                ++NumUnusedRefrig;
            }
        }

        NumUnusedGlycol = 0;

        for (Item = 1; Item <= state.dataFluidProps->NumOfGlycols; ++Item) {
            if (state.dataFluidProps->GlycolUsed(Item)) continue;
            if (UtilityRoutines::SameString(state.dataFluidProps->GlycolData(Item).Name, Water)) continue;
            if (UtilityRoutines::SameString(state.dataFluidProps->GlycolData(Item).Name, EthyleneGlycol)) continue;
            if (UtilityRoutines::SameString(state.dataFluidProps->GlycolData(Item).Name, PropyleneGlycol)) continue;
            if (NeedOrphanMessage && state.dataGlobal->DisplayUnusedObjects) {
                ShowWarningError(state, "The following fluid names are \"Unused Fluids\".  These fluids are in the idf");
                ShowContinueError(state, " file but are never obtained by the simulation and therefore are NOT used.");
                NeedOrphanMessage = false;
            }
            if (state.dataGlobal->DisplayUnusedObjects) {
                ShowMessage(state, "Glycol=" + state.dataFluidProps->GlycolData(Item).Name);
            } else {
                ++NumUnusedGlycol;
            }
        }

        if (NumUnusedRefrig > 0 || NumUnusedGlycol > 0) {
            if (NumUnusedRefrig > 0) ShowMessage(state, format("There are {} unused refrigerants in input.", NumUnusedRefrig));
            if (NumUnusedGlycol > 0) ShowMessage(state, format("There are {} unused glycols in input.", NumUnusedGlycol));
            ShowMessage(state, "Use Output:Diagnostics,DisplayUnusedObjects; to see them.");
        }
    }

    void ReportFatalGlycolErrors(EnergyPlusData &state,
                                 int const NumGlycols,               // Number of Glycols in input/data
                                 int const GlycolNum,                // Glycol Index
                                 bool const DataPresent,             // data is present for this fluid.
                                 std::string_view const GlycolName,  // Name being reported
                                 std::string_view const RoutineName, // Routine name to show
                                 std::string_view const Property,    // Property being requested
                                 std::string_view const CalledFrom   // original called from (external to fluid properties)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   July 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Consolidate fatal error reporting for glycols.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RefrigNo;

        // check and see if it might be a refrigerant
        RefrigNo = FindRefrigerant(state, GlycolName);

        if (NumGlycols == 0) {
            ShowSevereError(
                state,
                fmt::format(
                    "{}: no glycols found -- cannot evaluate fluid {} for \"{}\", called from: {}", RoutineName, Property, GlycolName, CalledFrom));
        } else if (GlycolNum == 0) {
            ShowSevereError(
                state, fmt::format("{}: data not found in input for requested glycol \"{}\", called from: {}", RoutineName, GlycolName, CalledFrom));
        } else if (!DataPresent) {
            ShowSevereError(state,
                            std::string{RoutineName} + ": " + std::string{Property} + " data not found in input for requested glycol \"" +
                                std::string{GlycolName} + fmt::format("\", called from: {}", CalledFrom));
        }
        if (RefrigNo > 0) ShowContinueError(state, "Note: that fluid is listed as a Refrigerant from input.");

        ShowFatalError(state, "Program terminates due to preceding condition.");
    }

    void ReportFatalRefrigerantErrors(EnergyPlusData &state,
                                      int const NumRefrigerants,              // Number of Refrigerants in input/data
                                      int const RefrigerantNum,               // Refrigerant Index
                                      bool const DataPresent,                 // data is present for this fluid.
                                      std::string_view const RefrigerantName, // Name being reported
                                      std::string_view const RoutineName,     // Routine name to show
                                      std::string_view const Property,        // Property being requested
                                      std::string_view const CalledFrom       // original called from (external to fluid properties)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   July 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Consolidate fatal error reporting for refrigerants.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int GlycolNo;

        // check and see if it might be a refrigerant
        GlycolNo = FindGlycol(state, RefrigerantName);

        if (NumRefrigerants == 0) {
            ShowSevereError(state,
                            fmt::format("{}: no refrigerants found -- cannot evaluate fluid {} for \"{}\", called from: {}",
                                        RoutineName,
                                        Property,
                                        RefrigerantName,
                                        CalledFrom));
        } else if (RefrigerantNum == 0) {
            ShowSevereError(
                state,
                fmt::format(
                    "{}: data not found in input for requested refrigerant \"{}\", called from: {}", RoutineName, RefrigerantName, CalledFrom));
        } else if (!DataPresent) {
            ShowSevereError(state,
                            fmt::format("{}: {} data not found in input for requested refrigerant \"{}\", called from: {}",
                                        RoutineName,
                                        Property,
                                        RefrigerantName,
                                        CalledFrom));
        }
        if (GlycolNo > 0) ShowContinueError(state, "Note: that fluid is listed as a Glycol from input.");

        ShowFatalError(state, "Program terminates due to preceding condition.");
    }

    void GetFluidDensityTemperatureLimits(EnergyPlusData &state, int const FluidIndex, Real64 &MinTempLimit, Real64 &MaxTempLimit)
    {

        // Get the input if we haven't already
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        if (FluidIndex > 0) {
            MinTempLimit = state.dataFluidProps->GlycolData(FluidIndex).RhoLowTempValue;
            MaxTempLimit = state.dataFluidProps->GlycolData(FluidIndex).RhoHighTempValue;
        }
    }

    void GetFluidSpecificHeatTemperatureLimits(EnergyPlusData &state, int const FluidIndex, Real64 &MinTempLimit, Real64 &MaxTempLimit)
    {
        // Get the input if we haven't already
        if (state.dataFluidProps->GetInput) {
            GetFluidPropertiesData(state);
            state.dataFluidProps->GetInput = false;
        }

        if (FluidIndex > 0) {
            MinTempLimit = state.dataFluidProps->GlycolData(FluidIndex).CpLowTempValue;
            MaxTempLimit = state.dataFluidProps->GlycolData(FluidIndex).CpHighTempValue;
        }
    }

    GlycolAPI::GlycolAPI(EnergyPlusData &state, std::string const &glycolName)
    {
        this->glycolName = EnergyPlus::UtilityRoutines::MakeUPPERCase(glycolName);
        this->glycolIndex = 0;
        this->cf = "GlycolAPI:Instance";
        if (this->glycolName != "WATER") {
            EnergyPlus::ShowFatalError(state, "Can only do water right now");
        }
    }
    Real64 GlycolAPI::specificHeat(EnergyPlusData &state, Real64 temperature)
    {
        return FluidProperties::GetSpecificHeatGlycol(state, this->glycolName, temperature, this->glycolIndex, this->cf);
    }
    Real64 GlycolAPI::density(EnergyPlusData &state, Real64 temperature)
    {
        return FluidProperties::GetDensityGlycol(state, this->glycolName, temperature, this->glycolIndex, this->cf);
    }
    Real64 GlycolAPI::conductivity(EnergyPlusData &state, Real64 temperature)
    {
        return FluidProperties::GetConductivityGlycol(state, this->glycolName, temperature, this->glycolIndex, this->cf);
    }
    Real64 GlycolAPI::viscosity(EnergyPlusData &state, Real64 temperature)
    {
        return FluidProperties::GetViscosityGlycol(state, this->glycolName, temperature, this->glycolIndex, this->cf);
    }

    RefrigerantAPI::RefrigerantAPI(EnergyPlusData &state, std::string const &refrigName)
    {
        this->rName = EnergyPlus::UtilityRoutines::MakeUPPERCase(refrigName);
        this->rIndex = 0;
        this->cf = "RefrigerantAPI:Instance";
        if (this->rName != "STEAM") {
            EnergyPlus::ShowFatalError(state, "Can only do steam right now");
        }
    }
    Real64 RefrigerantAPI::saturationPressure(EnergyPlusData &state, Real64 temperature)
    {
        return FluidProperties::GetSatPressureRefrig(state, this->rName, temperature, this->rIndex, this->cf);
    }
    Real64 RefrigerantAPI::saturationTemperature(EnergyPlusData &state, Real64 pressure)
    {
        return FluidProperties::GetSatTemperatureRefrig(state, this->rName, pressure, this->rIndex, this->cf);
    }
    Real64 RefrigerantAPI::saturatedEnthalpy(EnergyPlusData &state, Real64 temperature, Real64 quality)
    {
        return FluidProperties::GetSatEnthalpyRefrig(state, this->rName, temperature, quality, this->rIndex, this->cf);
    }
    Real64 RefrigerantAPI::saturatedDensity(EnergyPlusData &state, Real64 temperature, Real64 quality)
    {
        return FluidProperties::GetSatDensityRefrig(state, this->rName, temperature, quality, this->rIndex, this->cf);
    }
    Real64 RefrigerantAPI::saturatedSpecificHeat(EnergyPlusData &state, Real64 temperature, Real64 quality)
    {
        return FluidProperties::GetSatSpecificHeatRefrig(state, this->rName, temperature, quality, this->rIndex, this->cf);
    }
    Real64 RefrigerantAPI::superHeatedEnthalpy(EnergyPlusData &state, Real64 temperature, Real64 pressure)
    {
        return FluidProperties::GetSupHeatEnthalpyRefrig(state, this->rName, temperature, pressure, this->rIndex, this->cf);
    }
    Real64 RefrigerantAPI::superHeatedPressure(EnergyPlusData &state, Real64 temperature, Real64 enthalpy)
    {
        return FluidProperties::GetSupHeatPressureRefrig(state, this->rName, temperature, enthalpy, this->rIndex, this->cf);
    }
    Real64 RefrigerantAPI::superHeatedDensity(EnergyPlusData &state, Real64 temperature, Real64 pressure)
    {
        return FluidProperties::GetSupHeatDensityRefrig(state, this->rName, temperature, pressure, this->rIndex, this->cf);
    }

} // namespace FluidProperties

} // namespace EnergyPlus
