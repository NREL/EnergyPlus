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
    Array1D<cached_tsh> cached_t_sh; // DIMENSION(t_sh_cache_size)
#endif
    // Data Initializer Forward Declarations
    // See GetFluidPropertiesData "SUBROUTINE LOCAL DATA" for actual data.

    // MODULE SUBROUTINES:

    // Functions

    void InitializeGlycRoutines()
    {
#ifdef EP_cache_GlycolSpecificHeat
        cached_t_sh.allocate({0, t_sh_cache_size});
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
